# Monitor Templates

Monitor templates are used for developing new bug checkers within `eba`. These provide an easy-to-understand, relatively simple way of implementing bug checkers for the Linux kernel as state machines. 

## Overview 

All monitor template-based bug checkers must implement the following signature: 

```ocaml
module type AutomataSpec = sig
	(** A name to identify the checker *)
    val name : string
    (** The internal state of the monitor **)
	type state

	(** Checker's extended internal state **)
	type checker_state = {
		current_state: state;
        (* A trace of the path through the eba CFG *)
        trace: step list;
        (* All the matches leading to the final state of the monitor *)
        matches: step list;
        (* A kill region for filtering results and eliminating false positives *)
		kill_region: Regions.t
	}

    (** Indicates whether the current state needs more investigation/verification in order to be reported as a bug **)
	type result = Okay of checker_state | Uncertain of checker_state

    (** States **)
    (* The initial state the monitor is instantiated, e.g. "Unlocked" *)
    val initial_state : step -> AFun.t -> checker_state
    (* An indication of whether the current state is the accepting state for the monitor *)
    val is_accepting : checker_state -> bool
    (* An indication of whether effects will write to a memory location *)
    val does_write : effects -> checker_state -> bool
    (* An indication of whether the order of effects should be permuted before running the transition function in order to detect bugs in different orderings of effects *)
    val should_permute : bool
    (* An indication of whether the current state is an error state (a subset of final states for specifying more complex bug patterns ) *)
    val is_error : checker_state -> bool
    (* The kinds of effects the monitor accepts *)
    val transition_labels : mem_kind list
    (* Pretty printing for the checker_state *)
    val pp_checker_state : result -> SmartPrint.t
    (* Same as pp_checker_state, but instead as a string *)
    val checker_state_to_string : result -> string
    (* A filter for eliminating unwanted results, e.g. removal of Uncertain results *)
	val filter_results : result list -> result list

	(** The transition function for the monitor. State changes happen here based on the previous state and the effects found in the input *)
	val transition : result -> Effects.e list -> step -> result
end
```

For an example of a full implementation of this signature, see [checkAutomataDoubleLock.ml](../checkAutomataDoubleLock.ml)

The definition of when and how state changes should happen is implemented in the `transition` function. This can efficiently be implemented as a pattern match on what the previous state was, and what the incoming input is. A new state is then returned and preserved in the [checker](../automataChecker.ml) which is in charge of applying the input found in the `eba` CFG to the transition function. 

# The Monitor Template Runner [AutomataChecker](../automataChecker.ml)

Monitor template definitions are passed to the [AutomataChecker.ml](../automataChecker.ml). This checker takes an implementation of a monitor template, and the function under analysis. This function is passed to the AutomataChecker by the outer `eba` analysis logic. 

The AutomataChecker will explore the `eba` CFG and apply any effects found within a so-called `step` in the CFG, provided that these effects are in the `transition_labels` of the monitor template implementation. The resulting state of applying the transition function of the monitor with the effects found in the `step` is preserved in the AutomataChecker as a mapping from the region a monitor template operates on to the current state of that monitor. 

Whenever effects involving a previously untracked region are encountered, a new instance of the monitor template is instantiated in its initial state, and the transition function is then applied with these effects. 

After the application of the transition function of the monitor template and the new state has been found, the AutomataChecker will inline any uncertain results in an attempt to gain more information about the uncertain result. If this does not provide more concrete knowledge about whether a bug is present, the monitor template result is discarded. 

Finally, if a bug is discovered it will then be reported to the outer `eba` logic and presented to the user. 

# Adding a new monitor template implementation to `eba` 

Once a new monitor template has been implemented, it can be exposed to the outer `eba` logic in order to allow users to run the monitor template on files. 

This is done in [eba.ml](../eba.ml) in the form seen below.

``` ocaml
if checks.chk_automata_double_lock
	then 
		List.map (fun fd -> CheckAutomataDoubleLock.check fileAbs fd (Opts.Get.no_static())) fds
		|> List.flatten 
		|> CheckAutomataDoubleLock.filter_results 
		|> CheckAutomataDoubleLock.stringify_results
		|> L.of_list 
		|> print_bugs;
```

```ocaml
let check_automata_double_lock =
	let doc = "Check for double locking using automata" in
    Arg.(value & flag & info ["La"; "dlockaut"] ~doc)
```

```ocaml
... $ check_automata_double_unlock $ ... $ check_automata_double_lock $ check_automata_uaf ...
```

Creating a new monitor template implementation _should_ be the only thing required in order to check for new bug types within `eba`, but modifications or additions might be needed in [AutomataChecker.ml](../automataChecker.ml). This depends on the use case and complexity of the bug.

For a complete overview of the design and implementations of monitor templates, see [the Thesis](https://github.com/andersfischernielsen/Finding-Resource-Manipulation-Bugs-with-Monitor-Automata-on-the-Example-of-the-Linux-Kernel/blob/master/report.pdf) where they were initially defined.
