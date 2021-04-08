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
    | Red         (* The monitor sees a lock statement/effect *)
    | Green       (* The monitor sees an unlock statement/effect *)
    | RedGreen    (* The monitor sees both lock and unlock effects *)
    | Orange      (* The monitor is in a critical section *)
    | Black       (* The monitor is outside critical sections *)
    | BlackOrange (* Either inside or outside; either possible *)

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

    | ________, true,  false -> Red
    | ________, false, true  -> Green
    | ________, true,  true  -> RedGreen
    | Red,      false, false -> Orange
    | Green,    false, false -> Black
    | RedGreen, false, false -> BlackOrange
    | pred,     false, false -> pred

  ;;     (* ??? *)


  (* ignored in cfg printer *)
  let is_in_interesting_section _ : bool = true

  (* ignored in cfg printer *)
  let is_in_final_state _ : bool = true

  let string_of_state state =
    match state with
    | Orange 	   -> "orange"
    | Black 	   -> "black"
    | BlackOrange  -> "black|orange"
    | Red 	   -> "red"
    | Green 	   -> "green"
    | RedGreen     -> "red|green"

end

module Spec: CfgPrinter.PrinterSpec = SpecT

module Printer = CfgPrinter.Make(Spec)
include Printer
