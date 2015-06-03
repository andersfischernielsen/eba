
(* TODO: Create immutable wrapper *)

open Type

(** Mapping of variables to shapes, and locations to effects.
 *
 * We track the effects of key location in a C file,
 * those corresponding to C statements.
 *)
type t

val create : unit -> t

val add_var : t -> Cil.varinfo -> shape scheme -> unit

val add_vars : t -> (Cil.varinfo * shape scheme) list -> unit

val add_loc : t -> Cil.location -> Effects.t -> unit

val fv_of : t -> Vars.t

val zonk : t -> unit

val shape_of : t -> Cil.varinfo -> shape scheme

val effect_of : t -> Cil.location -> Effects.t

val pp : t -> PP.doc

val to_string : t -> string