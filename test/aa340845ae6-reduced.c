// SPDX-License-Identifier: GPL-2.0
/*
 * Shared application/kernel submission and completion ring pairs, for
 * supporting fast/efficient IO.
 *
 * A note on the read/write ordering memory barriers that are matched between
 * the application and kernel side.
 *
 * After the application reads the CQ ring tail, it must use an
 * appropriate smp_rmb() to pair with the smp_wmb() the kernel uses
 * before writing the tail (using smp_load_acquire to read the tail will
 * do). It also needs a smp_mb() before updating CQ head (ordering the
 * entry load(s) with the head store), pairing with an implicit barrier
 * through a control-dependency in io_get_cqring (smp_store_release to
 * store head will do). Failure to do so could lead to reading invalid
 * CQ entries.
 *
 * Likewise, the application must use an appropriate smp_wmb() before
 * writing the SQ tail (ordering SQ entry stores with the tail store),
 * which pairs with smp_load_acquire in io_get_sqring (smp_store_release
 * to store the tail will do). And it needs a barrier ordering the SQ
 * head load before writing new SQ entries (smp_load_acquire to read
 * head will do).
 *
 * When using the SQ poll thread (IORING_SETUP_SQPOLL), the application
 * needs to check the SQ flags for IORING_SQ_NEED_WAKEUP *after*
 * updating the SQ tail; a full memory barrier smp_mb() is needed
 * between.
 *
 * Also see the examples in the liburing library:
 *
 *	git://git.kernel.dk/liburing
 *
 * io_uring also uses READ/WRITE_ONCE() for _any_ store or load that happens
 * from data shared between the kernel and application. This is done both
 * for ordering purposes, but also to ensure that once a value is loaded from
 * data that the application could potentially modify, it remains stable.
 *
 * Copyright (C) 2018-2019 Jens Axboe
 * Copyright (c) 2018-2019 Christoph Hellwig
 */
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/errno.h>
#include <linux/syscalls.h>
#include <linux/compat.h>
#include <net/compat.h>
#include <linux/refcount.h>
#include <linux/uio.h>
#include <linux/bits.h>

#include <linux/sched/signal.h>
#include <linux/fs.h>
#include <linux/file.h>
#include <linux/fdtable.h>
#include <linux/mm.h>
#include <linux/mman.h>
#include <linux/percpu.h>
#include <linux/slab.h>
#include <linux/kthread.h>
#include <linux/blkdev.h>
#include <linux/bvec.h>
#include <linux/net.h>
#include <net/sock.h>
#include <net/af_unix.h>
#include <net/scm.h>
#include <linux/anon_inodes.h>
#include <linux/sched/mm.h>
#include <linux/uaccess.h>
#include <linux/nospec.h>
#include <linux/sizes.h>
#include <linux/hugetlb.h>
#include <linux/highmem.h>
#include <linux/namei.h>
#include <linux/fsnotify.h>
#include <linux/fadvise.h>
#include <linux/eventpoll.h>
#include <linux/fs_struct.h>
#include <linux/splice.h>
#include <linux/task_work.h>
#include <linux/pagemap.h>

#define CREATE_TRACE_POINTS
#include <trace/events/io_uring.h>

#include <uapi/linux/io_uring.h>

#include "internal.h"
#include "io-wq.h"

#define IORING_MAX_ENTRIES	32768
#define IORING_MAX_CQ_ENTRIES	(2 * IORING_MAX_ENTRIES)

/*
 * Shift of 9 is 512 entries, or exactly one page on 64-bit archs
 */
#define IORING_FILE_TABLE_SHIFT	9
#define IORING_MAX_FILES_TABLE	(1U << IORING_FILE_TABLE_SHIFT)
#define IORING_FILE_TABLE_MASK	(IORING_MAX_FILES_TABLE - 1)
#define IORING_MAX_FIXED_FILES	(64 * IORING_MAX_FILES_TABLE)

struct io_uring {
	u32 head ____cacheline_aligned_in_smp;
	u32 tail ____cacheline_aligned_in_smp;
};

/*
 * This data is shared with the application through the mmap at offsets
 * IORING_OFF_SQ_RING and IORING_OFF_CQ_RING.
 *
 * The offsets to the member fields are published through struct
 * io_sqring_offsets when calling io_uring_setup.
 */
struct io_rings {
	/*
	 * Head and tail offsets into the ring; the offsets need to be
	 * masked to get valid indices.
	 *
	 * The kernel controls head of the sq ring and the tail of the cq ring,
	 * and the application controls tail of the sq ring and the head of the
	 * cq ring.
	 */
	struct io_uring		sq, cq;
	/*
	 * Bitmasks to apply to head and tail offsets (constant, equals
	 * ring_entries - 1)
	 */
	u32			sq_ring_mask, cq_ring_mask;
	/* Ring sizes (constant, power of 2) */
	u32			sq_ring_entries, cq_ring_entries;
	/*
	 * Number of invalid entries dropped by the kernel due to
	 * invalid index stored in array
	 *
	 * Written by the kernel, shouldn't be modified by the
	 * application (i.e. get number of "new events" by comparing to
	 * cached value).
	 *
	 * After a new SQ head value was read by the application this
	 * counter includes all submissions that were dropped reaching
	 * the new SQ head (and possibly more).
	 */
	u32			sq_dropped;
	/*
	 * Runtime SQ flags
	 *
	 * Written by the kernel, shouldn't be modified by the
	 * application.
	 *
	 * The application needs a full memory barrier before checking
	 * for IORING_SQ_NEED_WAKEUP after updating the sq tail.
	 */
	u32			sq_flags;
	/*
	 * Runtime CQ flags
	 *
	 * Written by the application, shouldn't be modified by the
	 * kernel.
	 */
	u32                     cq_flags;
	/*
	 * Number of completion events lost because the queue was full;
	 * this should be avoided by the application by making sure
	 * there are not more requests pending than there is space in
	 * the completion queue.
	 *
	 * Written by the kernel, shouldn't be modified by the
	 * application (i.e. get number of "new events" by comparing to
	 * cached value).
	 *
	 * As completion events come in out of order this counter is not
	 * ordered with any other data.
	 */
	u32			cq_overflow;
	/*
	 * Ring buffer of completion events.
	 *
	 * The kernel writes completion events fresh every time they are
	 * produced, so the application is allowed to modify pending
	 * entries.
	 */
	struct io_uring_cqe	cqes[] ____cacheline_aligned_in_smp;
};

struct io_mapped_ubuf {
	u64		ubuf;
	size_t		len;
	struct		bio_vec *bvec;
	unsigned int	nr_bvecs;
};

struct fixed_file_table {
	struct file		**files;
};

struct fixed_file_ref_node {
	struct percpu_ref		refs;
	struct list_head		node;
	struct list_head		file_list;
	struct fixed_file_data		*file_data;
	struct llist_node		llist;
};

struct fixed_file_data {
	struct fixed_file_table		*table;
	struct io_ring_ctx		*ctx;

	struct percpu_ref		*cur_refs;
	struct percpu_ref		refs;
	struct completion		done;
	struct list_head		ref_list;
	spinlock_t			lock;
};

struct io_buffer {
	struct list_head list;
	__u64 addr;
	__s32 len;
	__u16 bid;
};

struct io_ring_ctx {
	struct {
		struct percpu_ref	refs;
	} ____cacheline_aligned_in_smp;

	struct {
		unsigned int		flags;
		unsigned int		compat: 1;
		unsigned int		limit_mem: 1;
		unsigned int		cq_overflow_flushed: 1;
		unsigned int		drain_next: 1;
		unsigned int		eventfd_async: 1;

		/*
		 * Ring buffer of indices into array of io_uring_sqe, which is
		 * mmapped by the application using the IORING_OFF_SQES offset.
		 *
		 * This indirection could e.g. be used to assign fixed
		 * io_uring_sqe entries to operations and only submit them to
		 * the queue when needed.
		 *
		 * The kernel modifies neither the indices array nor the entries
		 * array.
		 */
		u32			*sq_array;
		unsigned		cached_sq_head;
		unsigned		sq_entries;
		unsigned		sq_mask;
		unsigned		sq_thread_idle;
		unsigned		cached_sq_dropped;
		atomic_t		cached_cq_overflow;
		unsigned long		sq_check_overflow;

		struct list_head	defer_list;
		struct list_head	timeout_list;
		struct list_head	cq_overflow_list;

		wait_queue_head_t	inflight_wait;
		struct io_uring_sqe	*sq_sqes;
	} ____cacheline_aligned_in_smp;

	struct io_rings	*rings;

	/* IO offload */
	struct io_wq		*io_wq;
	struct task_struct	*sqo_thread;	/* if using sq thread polling */
	struct mm_struct	*sqo_mm;
	wait_queue_head_t	sqo_wait;

	/*
	 * If used, fixed file set. Writers must ensure that ->refs is dead,
	 * readers must ensure that ->refs is alive as long as the file* is
	 * used. Only updated through io_uring_register(2).
	 */
	struct fixed_file_data	*file_data;
	unsigned		nr_user_files;
	int 			ring_fd;
	struct file 		*ring_file;

	/* if used, fixed mapped user buffers */
	unsigned		nr_user_bufs;
	struct io_mapped_ubuf	*user_bufs;

	struct user_struct	*user;

	const struct cred	*creds;

	struct completion	ref_comp;
	struct completion	sq_thread_comp;

	/* if all else fails... */
	struct io_kiocb		*fallback_req;

#if defined(CONFIG_UNIX)
	struct socket		*ring_sock;
#endif

	struct idr		io_buffer_idr;

	struct idr		personality_idr;

	struct {
		unsigned		cached_cq_tail;
		unsigned		cq_entries;
		unsigned		cq_mask;
		atomic_t		cq_timeouts;
		unsigned long		cq_check_overflow;
		struct wait_queue_head	cq_wait;
		struct fasync_struct	*cq_fasync;
		struct eventfd_ctx	*cq_ev_fd;
	} ____cacheline_aligned_in_smp;

	struct {
		struct mutex		uring_lock;
		wait_queue_head_t	wait;
	} ____cacheline_aligned_in_smp;

	struct {
		spinlock_t		completion_lock;

		/*
		 * ->poll_list is protected by the ctx->uring_lock for
		 * io_uring instances that don't use IORING_SETUP_SQPOLL.
		 * For SQPOLL, only the single threaded io_sq_thread() will
		 * manipulate the list, hence no extra locking is needed there.
		 */
		struct list_head	poll_list;
		struct hlist_head	*cancel_hash;
		unsigned		cancel_hash_bits;
		bool			poll_multi_file;

		spinlock_t		inflight_lock;
		struct list_head	inflight_list;
	} ____cacheline_aligned_in_smp;

	struct delayed_work		file_put_work;
	struct llist_head		file_put_llist;

	struct work_struct		exit_work;
};

/*
 * First field must be the file pointer in all the
 * iocb unions! See also 'struct kiocb' in <linux/fs.h>
 */
struct io_poll_iocb {
	struct file			*file;
	union {
		struct wait_queue_head	*head;
		u64			addr;
	};
	__poll_t			events;
	bool				done;
	bool				canceled;
	struct wait_queue_entry		wait;
};

struct io_close {
	struct file			*file;
	struct file			*put_file;
	int				fd;
};

struct io_timeout_data {
	struct io_kiocb			*req;
	struct hrtimer			timer;
	struct timespec64		ts;
	enum hrtimer_mode		mode;
};

struct io_accept {
	struct file			*file;
	struct sockaddr __user		*addr;
	int __user			*addr_len;
	int				flags;
	unsigned long			nofile;
};

struct io_sync {
	struct file			*file;
	loff_t				len;
	loff_t				off;
	int				flags;
	int				mode;
};

struct io_cancel {
	struct file			*file;
	u64				addr;
};

struct io_timeout {
	struct file			*file;
	u64				addr;
	int				flags;
	u32				off;
	u32				target_seq;
};

struct io_rw {
	/* NOTE: kiocb has the file as the first member, so don't do it here */
	struct kiocb			kiocb;
	u64				addr;
	u64				len;
};

struct io_connect {
	struct file			*file;
	struct sockaddr __user		*addr;
	int				addr_len;
};

struct io_sr_msg {
	struct file			*file;
	union {
		struct user_msghdr __user *msg;
		void __user		*buf;
	};
	int				msg_flags;
	int				bgid;
	size_t				len;
	struct io_buffer		*kbuf;
};

struct io_open {
	struct file			*file;
	int				dfd;
	struct filename			*filename;
	struct open_how			how;
	unsigned long			nofile;
};

struct io_files_update {
	struct file			*file;
	u64				arg;
	u32				nr_args;
	u32				offset;
};

struct io_fadvise {
	struct file			*file;
	u64				offset;
	u32				len;
	u32				advice;
};

struct io_madvise {
	struct file			*file;
	u64				addr;
	u32				len;
	u32				advice;
};

struct io_epoll {
	struct file			*file;
	int				epfd;
	int				op;
	int				fd;
	struct epoll_event		event;
};

struct io_splice {
	struct file			*file_out;
	struct file			*file_in;
	loff_t				off_out;
	loff_t				off_in;
	u64				len;
	unsigned int			flags;
};

struct io_provide_buf {
	struct file			*file;
	__u64				addr;
	__s32				len;
	__u32				bgid;
	__u16				nbufs;
	__u16				bid;
};

struct io_statx {
	struct file			*file;
	int				dfd;
	unsigned int			mask;
	unsigned int			flags;
	const char __user		*filename;
	struct statx __user		*buffer;
};

struct io_async_connect {
	struct sockaddr_storage		address;
};

struct io_async_msghdr {
	struct iovec			fast_iov[UIO_FASTIOV];
	struct iovec			*iov;
	struct sockaddr __user		*uaddr;
	struct msghdr			msg;
	struct sockaddr_storage		addr;
};

struct io_async_rw {
	struct iovec			fast_iov[UIO_FASTIOV];
	struct iovec			*iov;
	ssize_t				nr_segs;
	ssize_t				size;
	struct wait_page_queue		wpq;
	struct callback_head		task_work;
};

struct io_async_ctx {
	union {
		struct io_async_rw	rw;
		struct io_async_msghdr	msg;
		struct io_async_connect	connect;
		struct io_timeout_data	timeout;
	};
};

enum {
	REQ_F_FIXED_FILE_BIT	= IOSQE_FIXED_FILE_BIT,
	REQ_F_IO_DRAIN_BIT	= IOSQE_IO_DRAIN_BIT,
	REQ_F_LINK_BIT		= IOSQE_IO_LINK_BIT,
	REQ_F_HARDLINK_BIT	= IOSQE_IO_HARDLINK_BIT,
	REQ_F_FORCE_ASYNC_BIT	= IOSQE_ASYNC_BIT,
	REQ_F_BUFFER_SELECT_BIT	= IOSQE_BUFFER_SELECT_BIT,

	REQ_F_LINK_HEAD_BIT,
	REQ_F_FAIL_LINK_BIT,
	REQ_F_INFLIGHT_BIT,
	REQ_F_CUR_POS_BIT,
	REQ_F_NOWAIT_BIT,
	REQ_F_LINK_TIMEOUT_BIT,
	REQ_F_ISREG_BIT,
	REQ_F_COMP_LOCKED_BIT,
	REQ_F_NEED_CLEANUP_BIT,
	REQ_F_OVERFLOW_BIT,
	REQ_F_POLLED_BIT,
	REQ_F_BUFFER_SELECTED_BIT,
	REQ_F_NO_FILE_TABLE_BIT,
	REQ_F_WORK_INITIALIZED_BIT,
	REQ_F_TASK_PINNED_BIT,

	/* not a real bit, just to check we're not overflowing the space */
	__REQ_F_LAST_BIT,
};

enum {
	/* ctx owns file */
	REQ_F_FIXED_FILE	= BIT(REQ_F_FIXED_FILE_BIT),
	/* drain existing IO first */
	REQ_F_IO_DRAIN		= BIT(REQ_F_IO_DRAIN_BIT),
	/* linked sqes */
	REQ_F_LINK		= BIT(REQ_F_LINK_BIT),
	/* doesn't sever on completion < 0 */
	REQ_F_HARDLINK		= BIT(REQ_F_HARDLINK_BIT),
	/* IOSQE_ASYNC */
	REQ_F_FORCE_ASYNC	= BIT(REQ_F_FORCE_ASYNC_BIT),
	/* IOSQE_BUFFER_SELECT */
	REQ_F_BUFFER_SELECT	= BIT(REQ_F_BUFFER_SELECT_BIT),

	/* head of a link */
	REQ_F_LINK_HEAD		= BIT(REQ_F_LINK_HEAD_BIT),
	/* fail rest of links */
	REQ_F_FAIL_LINK		= BIT(REQ_F_FAIL_LINK_BIT),
	/* on inflight list */
	REQ_F_INFLIGHT		= BIT(REQ_F_INFLIGHT_BIT),
	/* read/write uses file position */
	REQ_F_CUR_POS		= BIT(REQ_F_CUR_POS_BIT),
	/* must not punt to workers */
	REQ_F_NOWAIT		= BIT(REQ_F_NOWAIT_BIT),
	/* has linked timeout */
	REQ_F_LINK_TIMEOUT	= BIT(REQ_F_LINK_TIMEOUT_BIT),
	/* regular file */
	REQ_F_ISREG		= BIT(REQ_F_ISREG_BIT),
	/* completion under lock */
	REQ_F_COMP_LOCKED	= BIT(REQ_F_COMP_LOCKED_BIT),
	/* needs cleanup */
	REQ_F_NEED_CLEANUP	= BIT(REQ_F_NEED_CLEANUP_BIT),
	/* in overflow list */
	REQ_F_OVERFLOW		= BIT(REQ_F_OVERFLOW_BIT),
	/* already went through poll handler */
	REQ_F_POLLED		= BIT(REQ_F_POLLED_BIT),
	/* buffer already selected */
	REQ_F_BUFFER_SELECTED	= BIT(REQ_F_BUFFER_SELECTED_BIT),
	/* doesn't need file table for this request */
	REQ_F_NO_FILE_TABLE	= BIT(REQ_F_NO_FILE_TABLE_BIT),
	/* io_wq_work is initialized */
	REQ_F_WORK_INITIALIZED	= BIT(REQ_F_WORK_INITIALIZED_BIT),
	/* req->task is refcounted */
	REQ_F_TASK_PINNED	= BIT(REQ_F_TASK_PINNED_BIT),
};

struct async_poll {
	struct io_poll_iocb	poll;
	struct io_wq_work	work;
};

/*
 * NOTE! Each of the iocb union members has the file pointer
 * as the first entry in their struct definition. So you can
 * access the file pointer through any of the sub-structs,
 * or directly as just 'ki_filp' in this struct.
 */
struct io_kiocb {
	union {
		struct file		*file;
		struct io_rw		rw;
		struct io_poll_iocb	poll;
		struct io_accept	accept;
		struct io_sync		sync;
		struct io_cancel	cancel;
		struct io_timeout	timeout;
		struct io_connect	connect;
		struct io_sr_msg	sr_msg;
		struct io_open		open;
		struct io_close		close;
		struct io_files_update	files_update;
		struct io_fadvise	fadvise;
		struct io_madvise	madvise;
		struct io_epoll		epoll;
		struct io_splice	splice;
		struct io_provide_buf	pbuf;
		struct io_statx		statx;
	};

	struct io_async_ctx		*io;
	int				cflags;
	u8				opcode;
	/* polled IO has completed */
	u8				iopoll_completed;

	u16				buf_index;

	struct io_ring_ctx	*ctx;
	struct list_head	list;
	unsigned int		flags;
	refcount_t		refs;
	struct task_struct	*task;
	unsigned long		fsize;
	u64			user_data;
	u32			result;
	u32			sequence;

	struct list_head	link_list;

	struct list_head	inflight_entry;

	struct percpu_ref	*fixed_file_refs;

	union {
		/*
		 * Only commands that never go async can use the below fields,
		 * obviously. Right now only IORING_OP_POLL_ADD uses them, and
		 * async armed poll handlers for regular commands. The latter
		 * restore the work, if needed.
		 */
		struct {
			struct hlist_node	hash_node;
			struct async_poll	*apoll;
		};
		struct io_wq_work	work;
	};
	struct callback_head	task_work;
};

#define IO_IOPOLL_BATCH			8

struct io_comp_state {
	unsigned int		nr;
	struct list_head	list;
	struct io_ring_ctx	*ctx;
};

struct io_submit_state {
	struct blk_plug		plug;

	/*
	 * io_kiocb alloc cache
	 */
	void			*reqs[IO_IOPOLL_BATCH];
	unsigned int		free_reqs;

	/*
	 * Batch completion logic
	 */
	struct io_comp_state	comp;

	/*
	 * File reference cache
	 */
	struct file		*file;
	unsigned int		fd;
	unsigned int		has_refs;
	unsigned int		used_refs;
	unsigned int		ios_left;
};

struct io_op_def {
	/* needs req->io allocated for deferral/async */
	unsigned		async_ctx : 1;
	/* needs current->mm setup, does mm access */
	unsigned		needs_mm : 1;
	/* needs req->file assigned */
	unsigned		needs_file : 1;
	/* don't fail if file grab fails */
	unsigned		needs_file_no_error : 1;
	/* hash wq insertion if file is a regular file */
	unsigned		hash_reg_file : 1;
	/* unbound wq insertion if file is a non-regular file */
	unsigned		unbound_nonreg_file : 1;
	/* opcode is not supported by this kernel */
	unsigned		not_supported : 1;
	/* needs file table */
	unsigned		file_table : 1;
	/* needs ->fs */
	unsigned		needs_fs : 1;
	/* set if opcode supports polled "wait" */
	unsigned		pollin : 1;
	unsigned		pollout : 1;
	/* op supports buffer selection */
	unsigned		buffer_select : 1;
};

static const struct io_op_def io_op_defs[] = {
	[IORING_OP_NOP] = {},
	[IORING_OP_READV] = {
		.async_ctx		= 1,
		.needs_mm		= 1,
		.needs_file		= 1,
		.unbound_nonreg_file	= 1,
		.pollin			= 1,
		.buffer_select		= 1,
	},
	[IORING_OP_WRITEV] = {
		.async_ctx		= 1,
		.needs_mm		= 1,
		.needs_file		= 1,
		.hash_reg_file		= 1,
		.unbound_nonreg_file	= 1,
		.pollout		= 1,
	},
	[IORING_OP_FSYNC] = {
		.needs_file		= 1,
	},
	[IORING_OP_READ_FIXED] = {
		.needs_file		= 1,
		.unbound_nonreg_file	= 1,
		.pollin			= 1,
	},
	[IORING_OP_WRITE_FIXED] = {
		.needs_file		= 1,
		.hash_reg_file		= 1,
		.unbound_nonreg_file	= 1,
		.pollout		= 1,
	},
	[IORING_OP_POLL_ADD] = {
		.needs_file		= 1,
		.unbound_nonreg_file	= 1,
	},
	[IORING_OP_POLL_REMOVE] = {},
	[IORING_OP_SYNC_FILE_RANGE] = {
		.needs_file		= 1,
	},
	[IORING_OP_SENDMSG] = {
		.async_ctx		= 1,
		.needs_mm		= 1,
		.needs_file		= 1,
		.unbound_nonreg_file	= 1,
		.needs_fs		= 1,
		.pollout		= 1,
	},
	[IORING_OP_RECVMSG] = {
		.async_ctx		= 1,
		.needs_mm		= 1,
		.needs_file		= 1,
		.unbound_nonreg_file	= 1,
		.needs_fs		= 1,
		.pollin			= 1,
		.buffer_select		= 1,
	},
	[IORING_OP_TIMEOUT] = {
		.async_ctx		= 1,
		.needs_mm		= 1,
	},
	[IORING_OP_TIMEOUT_REMOVE] = {},
	[IORING_OP_ACCEPT] = {
		.needs_mm		= 1,
		.needs_file		= 1,
		.unbound_nonreg_file	= 1,
		.file_table		= 1,
		.pollin			= 1,
	},
	[IORING_OP_ASYNC_CANCEL] = {},
	[IORING_OP_LINK_TIMEOUT] = {
		.async_ctx		= 1,
		.needs_mm		= 1,
	},
	[IORING_OP_CONNECT] = {
		.async_ctx		= 1,
		.needs_mm		= 1,
		.needs_file		= 1,
		.unbound_nonreg_file	= 1,
		.pollout		= 1,
	},
	[IORING_OP_FALLOCATE] = {
		.needs_file		= 1,
	},
	[IORING_OP_OPENAT] = {
		.file_table		= 1,
		.needs_fs		= 1,
	},
	[IORING_OP_CLOSE] = {
		.needs_file		= 1,
		.needs_file_no_error	= 1,
		.file_table		= 1,
	},
	[IORING_OP_FILES_UPDATE] = {
		.needs_mm		= 1,
		.file_table		= 1,
	},
	[IORING_OP_STATX] = {
		.needs_mm		= 1,
		.needs_fs		= 1,
		.file_table		= 1,
	},
	[IORING_OP_READ] = {
		.needs_mm		= 1,
		.needs_file		= 1,
		.unbound_nonreg_file	= 1,
		.pollin			= 1,
		.buffer_select		= 1,
	},
	[IORING_OP_WRITE] = {
		.needs_mm		= 1,
		.needs_file		= 1,
		.unbound_nonreg_file	= 1,
		.pollout		= 1,
	},
	[IORING_OP_FADVISE] = {
		.needs_file		= 1,
	},
	[IORING_OP_MADVISE] = {
		.needs_mm		= 1,
	},
	[IORING_OP_SEND] = {
		.needs_mm		= 1,
		.needs_file		= 1,
		.unbound_nonreg_file	= 1,
		.pollout		= 1,
	},
	[IORING_OP_RECV] = {
		.needs_mm		= 1,
		.needs_file		= 1,
		.unbound_nonreg_file	= 1,
		.pollin			= 1,
		.buffer_select		= 1,
	},
	[IORING_OP_OPENAT2] = {
		.file_table		= 1,
		.needs_fs		= 1,
	},
	[IORING_OP_EPOLL_CTL] = {
		.unbound_nonreg_file	= 1,
		.file_table		= 1,
	},
	[IORING_OP_SPLICE] = {
		.needs_file		= 1,
		.hash_reg_file		= 1,
		.unbound_nonreg_file	= 1,
	},
	[IORING_OP_PROVIDE_BUFFERS] = {},
	[IORING_OP_REMOVE_BUFFERS] = {},
	[IORING_OP_TEE] = {
		.needs_file		= 1,
		.hash_reg_file		= 1,
		.unbound_nonreg_file	= 1,
	},
};

enum io_mem_account {
	ACCT_LOCKED,
	ACCT_PINNED,
};

static bool io_rw_reissue(struct io_kiocb *req, long res);
static void io_cqring_fill_event(struct io_kiocb *req, long res);
static void io_put_req(struct io_kiocb *req);
static void io_double_put_req(struct io_kiocb *req);
static void __io_double_put_req(struct io_kiocb *req);
static struct io_kiocb *io_prep_linked_timeout(struct io_kiocb *req);
static void io_queue_linked_timeout(struct io_kiocb *req);
static int __io_sqe_files_update(struct io_ring_ctx *ctx,
				 struct io_uring_files_update *ip,
				 unsigned nr_args);
static int io_grab_files(struct io_kiocb *req);
static void io_complete_rw_common(struct kiocb *kiocb, long res,
				  struct io_comp_state *cs);
static void io_cleanup_req(struct io_kiocb *req);
static int io_file_get(struct io_submit_state *state, struct io_kiocb *req,
		       int fd, struct file **out_file, bool fixed);
static void __io_queue_sqe(struct io_kiocb *req,
			   const struct io_uring_sqe *sqe,
			   struct io_comp_state *cs);

static ssize_t io_import_iovec(int rw, struct io_kiocb *req,
			       struct iovec **iovec, struct iov_iter *iter,
			       bool needs_lock);
static int io_setup_async_rw(struct io_kiocb *req, ssize_t io_size,
			     struct iovec *iovec, struct iovec *fast_iov,
			     struct iov_iter *iter);

static struct kmem_cache *req_cachep;

static const struct file_operations io_uring_fops;

struct sock *io_uring_get_socket(struct file *file)
{
#if defined(CONFIG_UNIX)
	if (file->f_op == &io_uring_fops) {
		struct io_ring_ctx *ctx = file->private_data;

		return ctx->ring_sock->sk;
	}
#endif
	return NULL;
}
EXPORT_SYMBOL(io_uring_get_socket);

/*
 * Must only be used if we don't need to care about links, usually from
 * within the completion handling itself.
 */
static void __io_double_put_req(struct io_kiocb *req)
{
	/* drop both submit and complete references */
	if (refcount_sub_and_test(2, &req->refs))
		__io_free_req(req);
}

static void io_double_put_req(struct io_kiocb *req)
{
	/* drop both submit and complete references */
	if (refcount_sub_and_test(2, &req->refs))
		io_free_req(req);
}

static inline void req_set_fail_links(struct io_kiocb *req)
{
	if ((req->flags & (REQ_F_LINK | REQ_F_HARDLINK)) == REQ_F_LINK)
		req->flags |= REQ_F_FAIL_LINK;
}

static inline bool io_should_trigger_evfd(struct io_ring_ctx *ctx)
{
	if (!ctx->cq_ev_fd)
		return false;
	if (READ_ONCE(ctx->rings->cq_flags) & IORING_CQ_EVENTFD_DISABLED)
		return false;
	if (!ctx->eventfd_async)
		return true;
	return io_wq_current_is_worker();
}

static void io_cqring_ev_posted(struct io_ring_ctx *ctx)
{
	if (waitqueue_active(&ctx->wait))
		wake_up(&ctx->wait);
	if (waitqueue_active(&ctx->sqo_wait))
		wake_up(&ctx->sqo_wait);
	if (io_should_trigger_evfd(ctx))
		eventfd_signal(ctx->cq_ev_fd, 1);
}

static void __io_queue_async_work(struct io_kiocb *req)
{
	struct io_ring_ctx *ctx = req->ctx;
	struct io_kiocb *link = io_prep_linked_timeout(req);

	trace_io_uring_queue_async_work(ctx, io_wq_is_hashed(&req->work), req,
					&req->work, req->flags);
	io_wq_enqueue(ctx->io_wq, &req->work);

	if (link)
		io_queue_linked_timeout(link);
}

static void __io_queue_deferred(struct io_ring_ctx *ctx)
{
	do {
		struct io_kiocb *req = list_first_entry(&ctx->defer_list,
							struct io_kiocb, list);

		if (req_need_defer(req))
			break;
		list_del_init(&req->list);
		/* punt-init is done before queueing for defer */
		__io_queue_async_work(req);
	} while (!list_empty(&ctx->defer_list));
}

static inline bool io_is_timeout_noseq(struct io_kiocb *req)
{
	return !req->timeout.off;
}

static void __io_commit_cqring(struct io_ring_ctx *ctx)
{
	struct io_rings *rings = ctx->rings;

	/* order cqe stores with ring update */
	smp_store_release(&rings->cq.tail, ctx->cached_cq_tail);

	if (wq_has_sleeper(&ctx->cq_wait)) {
		wake_up_interruptible(&ctx->cq_wait);
		kill_fasync(&ctx->cq_fasync, SIGIO, POLL_IN);
	}
}

static void io_flush_timeouts(struct io_ring_ctx *ctx)
{
	while (!list_empty(&ctx->timeout_list)) {
		struct io_kiocb *req = list_first_entry(&ctx->timeout_list,
							struct io_kiocb, list);

		if (io_is_timeout_noseq(req))
			break;
		if (req->timeout.target_seq != ctx->cached_cq_tail
					- atomic_read(&ctx->cq_timeouts))
			break;

		list_del_init(&req->list);
		io_kill_timeout(req);
	}
}

static void io_commit_cqring(struct io_ring_ctx *ctx)
{
	io_flush_timeouts(ctx);
	__io_commit_cqring(ctx);

	if (unlikely(!list_empty(&ctx->defer_list)))
		__io_queue_deferred(ctx);
}

static void __io_req_task_cancel(struct io_kiocb *req, int error)
{
	struct io_ring_ctx *ctx = req->ctx;

	spin_lock_irq(&ctx->completion_lock);
	io_cqring_fill_event(req, error);
	io_commit_cqring(ctx);
	spin_unlock_irq(&ctx->completion_lock);

	io_cqring_ev_posted(ctx);
	req_set_fail_links(req);
	io_double_put_req(req);
}

static int __io_sq_thread_acquire_mm(struct io_ring_ctx *ctx)
{
	if (!current->mm) {
		if (unlikely(!ctx->sqo_mm || !mmget_not_zero(ctx->sqo_mm)))
			return -EFAULT;
		kthread_use_mm(ctx->sqo_mm);
	}

	return 0;
}

static void __io_req_task_submit(struct io_kiocb *req)
{
	struct io_ring_ctx *ctx = req->ctx;

	if (!__io_sq_thread_acquire_mm(ctx)) {
		mutex_lock(&ctx->uring_lock);
		__io_queue_sqe(req, NULL, NULL);
		mutex_unlock(&ctx->uring_lock);
	} else {
		__io_req_task_cancel(req, -EFAULT);
	}
}

static bool io_poll_rewait(struct io_kiocb *req, struct io_poll_iocb *poll)
	__acquires(&req->ctx->completion_lock)
{
	struct io_ring_ctx *ctx = req->ctx;

	if (!req->result && !READ_ONCE(poll->canceled)) {
		struct poll_table_struct pt = { ._key = poll->events };

		req->result = vfs_poll(req->file, &pt) & poll->events;
	}

	spin_lock_irq(&ctx->completion_lock);
	if (!req->result && !READ_ONCE(poll->canceled)) {
		add_wait_queue(poll->head, &poll->wait);
		return true;
	}

	return false;
}

static void io_async_task_func(struct callback_head *cb)
{
	struct io_kiocb *req = container_of(cb, struct io_kiocb, task_work);
	struct async_poll *apoll = req->apoll;
	struct io_ring_ctx *ctx = req->ctx;

	trace_io_uring_task_run(req->ctx, req->opcode, req->user_data);

	if (io_poll_rewait(req, &apoll->poll)) {
		spin_unlock_irq(&ctx->completion_lock);
		return;
	}

	/* If req is still hashed, it cannot have been canceled. Don't check. */
	if (hash_hashed(&req->hash_node))
		hash_del(&req->hash_node);

	spin_unlock_irq(&ctx->completion_lock);

	/* restore ->work in case we need to retry again */
	if (req->flags & REQ_F_WORK_INITIALIZED)
		memcpy(&req->work, &apoll->work, sizeof(req->work));

	if (!READ_ONCE(apoll->poll.canceled))
		__io_req_task_submit(req);
	else
		__io_req_task_cancel(req, -ECANCELED);

	kfree(apoll);
}
