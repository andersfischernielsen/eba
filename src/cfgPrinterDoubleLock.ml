open Batteries
open Type
open Effects

module Spec: CfgPrinter.PrinterSpec = struct

    type state =
		| Initial
		| Orange
        | Black

    let initial_state = Initial
    let transition_labels = [Lock; Unlock]

    let is_in_transition_labels effect =
		match effect with
		| Mem(kind, _) -> List.mem kind transition_labels
		| _ -> false

    let transition current input =
		match current with
		| Initial ->
			(match input with
			| [Mem(Lock, _)]		-> Orange
			| [Mem(Lock, _); _]		-> Orange
			| [_; Mem(Lock, _)] 	-> Orange
			| _         		    -> current
			)
        | Orange ->
			(match input with
			| [Mem(Unlock, _)]		-> Black
			| [_; Mem(Unlock, _)]	-> Black
			| [Mem(Unlock, _); _]	-> Black
			| _         			-> current
			)
        | Black 					-> Initial

    let is_in_interesting_section state =
		match state with
        | Orange 	-> true
        | Black 	-> true
	| Initial 	-> true

	let is_in_final_state state =
		match state with
        | Black 	-> true
		| _ 		-> false

	let string_of_state state =
		match state with
		| Orange 	-> Format.sprintf "Locked"
		| Black 	-> Format.sprintf "Unlocked"
		| _ 		-> ""
end

module Printer = CfgPrinter.Make(Spec)
include Printer
