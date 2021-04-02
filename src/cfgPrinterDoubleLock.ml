open Batteries
open Type
open Effects

(** A monitor that attempts to mark critical sections on a control flow graph.
    It is assumed that it is run on correct code, so it can do this
    unambigously.  Since this is  a big assumption (a CFG is too coarse an
    abstraction), it tries to recover heuristically after experiencing
    conflicts (lock-lock, unlock-unlock) or nondeterminism.

    The state space is a powerset of the states of a regular monitor. *)
module SpecT = struct

  type state =
    | Orange      (* The monitor is in a critical section *)
    | Black       (* The monitor is outside critical sections *)
    | BlackOrange (* Either inside or outside; either possible *)
    | Confused    (* Seen a bug, double lock/unlock, trying to recover *)

  let initial_state = Black
  let transition_labels = [Lock; Unlock]

  let locks effects: bool =
    List.exists (function Mem (Lock, _) -> true | _ -> false) effects ;;

  let unlocks effects: bool =
    List.exists (function Mem (Unlock, _) -> true | _ -> false) effects ;;

  let is_in_transition_labels (effect: Effects.e): bool =
    match effect with
    | Mem(kind, _) -> List.mem kind transition_labels
    | ____________ -> false

  (* This transition relation is 'resilient'. It tries not to crash,
     but extract info for learning as much as possible. When confused
     restarts, it is meant to be run on error free files.
     Confused means that we found an error, which we should not if cfg
     printer is only used in learning on correct files. We restart
     when this happens. *)
  let transition current (effect: Effects.e list): state =
    match current, locks effect, unlocks effect with

    | Black, false, true -> Confused
    | Black, true, false -> Orange
    | Black, false, false -> Black
    | Black, true, true -> BlackOrange

    | Orange, false, true -> Black
    | Orange, true, false -> Confused
    | Orange, false, false -> Orange
    | Orange, true, true -> Black

    | Confused, true, false -> Orange
    | Confused, false, true -> Black
    | Confused, true, true -> Orange
    | Confused, false, false -> Black

    | BlackOrange, true, true -> BlackOrange
    | BlackOrange, false, false -> BlackOrange
    | BlackOrange, true, false -> Orange       (* ??? *)
    | BlackOrange, false, true -> Black ;;     (* ??? *)


  (* ignored in cfg printer *)
  let is_in_interesting_section _ : bool = true

  (* ignored in cfg printer *)
  let is_in_final_state _ : bool = true

  let string_of_state state =
    match state with
    | Orange 	   -> "orange"
    | Black 	   -> "black"
    | BlackOrange  -> "black|orange"
    | Confused     -> "confused" (* should never happen *)

end

module Spec: CfgPrinter.PrinterSpec = SpecT

module Printer = CfgPrinter.Make(Spec)
include Printer
