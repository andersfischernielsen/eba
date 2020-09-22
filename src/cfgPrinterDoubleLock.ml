open Batteries
open Type
open PathTree
open Effects
open CfgPrinter

module Spec: PrinterSpec = struct 
    type state = 
		| Initial
		| Locked
        | Unlocked
    
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
			| [Mem(Lock, _)]		-> Locked
			| [Mem(Lock, _); _]		-> Locked
			| [_; Mem(Lock, _)] 	-> Locked
			| _         		    -> current
			)
        | Locked ->
			(match input with 
			| [Mem(Unlock, _)]		-> Unlocked
			| [_; Mem(Unlock, _)]	-> Unlocked
			| [Mem(Unlock, _); _]	-> Unlocked
			| _         			-> current
			)
        | Unlocked 					-> Initial

    let is_in_interesting_section state = 
		match state with 
        | Locked 	-> true
        | Unlocked 	-> true
		| Initial 	-> false

	let is_in_final_state state = 
		match state with 
        | Unlocked 	-> true
		| _ 		-> false

	let string_of_state state s = 
		match state with 
		| Locked 	-> Format.sprintf "%s: Locked" s 
		| Unlocked 	-> Format.sprintf "%s: Unlocked" s 
		| _ 		-> ""
end 

module Printer = Make(Spec)
include Printer