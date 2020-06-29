open Type
open PathTree
open Effects
open Random
open Batteries

module type LockingAutomataHelperSpec = sig
    type state
    type checker_state
    val transition_labels : mem_kind list
    val does_write : effects -> Regions.t -> bool
    val filter_results : checker_state list -> checker_state list
    val pp_checker_state : checker_state -> name -> SmartPrint.t
end

module Helper(A : AutomataChecker.AutomataSpec) : LockingAutomataHelperSpec = struct
    type state = A.state
    type checker_state = A.checker_state

    let transition_labels = [Lock; Unlock] 

    let does_write effects kill_region = 
        Regions.exists (fun r -> (E.(mem (writes ~r) effects))) kill_region
    
    let filter_results (matches:checker_state list) = 
        let inner = fun (a:checker_state) (b:checker_state) -> (Int.compare (List.length a.trace) (List.length b.trace)) in
        let sorted = List.sort inner matches in 

        let trace_repeats l = match l with
            | [] | [_] -> false
            | (head::tail) -> List.exists (fun t -> Cil.compareLoc head.sloc t.sloc = 0) tail in

        let no_duplicate_regions = List.fold_right (fun (current:checker_state) acc -> 
            if List.exists (fun e -> Set.mem e (snd acc)) current.matches || trace_repeats current.trace
            then acc (* If a step has already been detected, skip it. *) 
            else (* Otherwise, include it. *)
                let union = Set.union (Set.of_list current.matches) (snd acc) in
                (current::(fst acc), union))
        sorted ([], Set.empty) in 

        fst no_duplicate_regions

    let pp_checker_state (state:checker_state) (name:string) = 
        let open PP in 
        let matches = List.rev state.matches in 
        let trace = List.rev state.trace in 
        let match_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline matches in
        let trace_locations = List.fold_left (fun acc m -> acc ++ Utils.Location.pp m.sloc ++ words (string_of_step m) + newline) newline trace in
        
        brackets (!^ name) + newline + newline
        ++ words "at:" ++ match_locations + newline
        ++ words "trace:" ++ trace_locations + newline
end