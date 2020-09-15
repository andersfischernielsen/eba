open Batteries

module L = LazyList

open Type
open Abs
open PathTree
open Effects
open PathTree
open Dolog

module type PrinterSpec = sig
	type state
	val transition: state -> e list -> state
    val should_stop_printing: state -> bool
    val initial_state: state
	val is_in_transition_labels: e -> bool
end

module type Printer = sig
	val print : AFile.t -> Cil.fundec -> unit
end

module Make(P: PrinterSpec) : Printer = struct
	let get_region e = 
		match e with
		| Mem(_, region) -> Some (region, e)
		| _ 			 -> None

	let extract_regions r_es = 
		let split = List.split r_es in
		(List.hd (fst split), (snd split))

    let rec explore_paths path map = 
		let p = path() in
		match p with
		| Seq(step, remaining) -> 
			let apply_transition effects (map_to_add_to:(region, P.state list) BatMap.t) = 
					let result = List.fold_right (fun (r_e:region * e list) map -> 
						match r_e with | r, es -> 
							(* Find the previous result if present, then determine new checker_state. *)
							let initial : P.state list = [P.initial_state] in 
							let result : P.state list = Map.find_default initial r map_to_add_to in 
							let applied = List.map (fun (s: P.state) -> P.transition s es) result 
							in
							Map.add r applied map) effects map_to_add_to in
					result
				in

			let input = EffectSet.filter P.is_in_transition_labels step.effs.may |> EffectSet.to_list in
			if (not (List.is_empty input)) then
				let region_options = List.map get_region input in
				let regions = List.fold_right (fun e acc -> (match e with Some r -> r::acc | None -> acc)) region_options [] in 
				
				let grouped = List.group (fun r r' -> Region.compare (fst r) (fst r')) regions |> List.map extract_regions in

				let states = apply_transition grouped map in 

				let should_stop = Map.exists (fun _ b -> List.exists (fun s -> P.should_stop_printing s) b) states in
				if not (should_stop)
				then 
					(Format.printf "%s:\n%s " (Utils.Location.pp step.sloc |> PP.to_string) (pp_step step |> PP.to_string);
					List.iter (fun e -> pp_e e |> PP.to_string |> Format.printf "%s ") (EffectSet.to_list step.effs.may);
					Format.printf "\n\n");
			
			explore_paths remaining states
		| Assume(_, _, remaining) -> 
			explore_paths remaining map
		| If(true_path, false_path) -> 
			explore_paths true_path map;
            explore_paths false_path map
		| Nil -> ()

	let print file declaration =
		let variable_info = Cil.(declaration.svar) in
        let _, global_function = Option.get(AFile.find_fun file variable_info) in
        let path_tree = paths_of global_function in
        explore_paths path_tree Map.empty
end