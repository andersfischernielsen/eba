open Batteries
open Type
open Effects

module Spec: CfgPrinter.PrinterSpec = struct

  type state = Red | Black

  let initial_state = Black
  let transition_labels = [Lock; Unlock]

  let is_in_transition_labels (effect: Effects.e): bool =
    match effect with
    | Mem(kind, _) -> List.mem kind transition_labels
    | ____________ -> false

  let transition current (input: Effects.e list): state =
    match current, input with
    | Black, [Mem(Lock, _)]    -> Red
    | Black, [Mem(Lock, _); _] -> Red
    | Black, [_; Mem(Lock, _)] -> Red
    | Red, [Mem(Unlock, _)]    -> Black
    | Red, [_; Mem(Unlock, _)] -> Black
    | Red, [Mem(Unlock, _); _] -> Black
    | ________________________ -> current

  (* ignored in cfg printer *)
  let is_in_interesting_section _ : bool = true

  (* ignored in cfg printer *)
  let is_in_final_state _ : bool = false

  let string_of_state state =
    match state with
    | Red 	-> "orange"
    | Black 	-> "black"

end

module Printer = CfgPrinter.Make(Spec)
include Printer
