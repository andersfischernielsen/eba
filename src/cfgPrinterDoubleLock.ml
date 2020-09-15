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
    let transition previous input = 
		match previous with
		| Initial ->
			(match input with 
			| [Mem(Lock, _); _]		-> Locked
			| [_; Mem(Lock, _)] 	-> Locked
			| _         		    -> previous
			)
        | Locked ->
			(match input with 
			| [_; Mem(Unlock, _)]	-> Unlocked
			| [Mem(Unlock, _); _]	-> Unlocked
			| _         			-> previous
			)
        | Unlocked 					-> Initial

    let should_stop_printing state = 
		match state with 
        | Unlocked 	-> true
        | _ 		-> false

end 

module Printer = Make(Spec)
include Printer