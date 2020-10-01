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
    val is_in_interesting_section: state -> bool
    val initial_state: state
	val is_in_transition_labels: e -> bool
	val is_in_final_state: state -> bool
	val string_of_state: state -> string -> string
end

module type Printer = sig
	val print : AFile.t -> Cil.fundec -> int -> unit
end

module Make(P: PrinterSpec) : Printer = struct
	let get_region e = 
		match e with
		| Mem(_, region) -> Some (region, e)
		| _ 			 -> None

	let extract_regions r_es = 
		let split = List.split r_es in
		(List.hd (fst split), (snd split))

	let find_variable r map func = 
		let found = Map.Exceptionless.find r map in 
		match found with 
		| Some (name, type_) -> func type_ name 
		| _ 		-> ()

	let generate_state_region_string region state = P.string_of_state state (Region.pp region |> PP.to_string)

    let rec explore_paths path func map var_region_map inline_limit = 
		let p = path() in
		match p with
		| Seq(step, remaining) -> 
			let apply_transition effects map_to_add_to = 
					let result = List.fold_right (fun (r_e:region * e list) map -> 
						match r_e with | r, es -> 
							let initial : P.state list = [P.initial_state] in 
							let result : P.state list = Map.find_default initial r map_to_add_to in 
							let applied = List.map (fun s -> P.transition s es) result 
							in
							Map.add r applied map) effects map_to_add_to in
					result
				in

			let call_present = find_in_stmt (fun is -> 
				if List.exists (fun i -> 
					match i with 
					| Cil.(Call _) -> true 
					| _ -> false) is 
				then Some(true) 
				else None)
				step 
			in

			if Option.is_some call_present && inline_limit > 0
			then 
				let inlined = inline func step in

				match inlined with 
				| Some (_, res) -> explore_paths res func map var_region_map (inline_limit-1)
				| _ -> ()
			else

			Printf.fprintf IO.stdout "";
			let input = step.effs.may |> EffectSet.to_list in
			let region_options = List.map get_region input in
			let regions = List.fold_right (fun e acc -> (match e with Some r -> r::acc | None -> acc)) region_options [] in 
			
			let grouped = List.group (fun r r' -> Region.compare (fst r) (fst r')) regions |> List.map extract_regions in

			let states = apply_transition grouped map in 

			let interesting_monitors = Map.filter (fun _ b -> List.exists (fun s -> P.is_in_interesting_section s) b) states in

			if not (Map.is_empty interesting_monitors)
			then 
				(Map.iter (fun k v -> List.iter (fun s -> Printf.fprintf IO.stdout "{ State: %s } " (generate_state_region_string k s)) v) interesting_monitors;
				Printf.fprintf IO.stdout "\n");
			
			let ints = enum_regions step.effs |> List.of_enum |> List.map (fun r -> Region.uniq_of r |> Uniq.to_int) in

			Printf.fprintf IO.stdout "%s:\n%s " (Utils.Location.pp step.sloc |> PP.to_string) (pp_step step |> PP.to_string);
			Printf.fprintf IO.stdout "\n";
			List.iter (fun r -> find_variable r var_region_map (Printf.fprintf IO.stdout "{ Reference: %s%s } ")) ints;
			Printf.fprintf IO.stdout "\n";
			List.iter (fun e -> pp_e e |> PP.to_string |> Printf.fprintf IO.stdout "{ Effect: %s } ") (EffectSet.to_list step.effs.may);
			Printf.fprintf IO.stdout "\n\n";
			
			let without_monitors_in_final_states = Map.map (fun state_list -> List.filter (fun s -> not (P.is_in_final_state s)) state_list) states in
			explore_paths remaining func without_monitors_in_final_states var_region_map inline_limit
		| Assume(_, _, remaining) -> 
			explore_paths remaining func map var_region_map inline_limit
		| If(true_path, false_path) -> 
			explore_paths true_path func map var_region_map inline_limit;
            explore_paths false_path func map var_region_map inline_limit
		| Nil -> ()

	let print file declaration inline_limit =
		let variable_info = Cil.(declaration.svar) in
        let _, global_function = Option.get(AFile.find_fun file variable_info) in
		Printf.fprintf IO.stdout "%s:%s:%i:\n" variable_info.vdecl.file variable_info.vname  variable_info.vdecl.line;
        let path_tree = paths_of global_function in

		let var_region_map = Map.foldi (fun (k:Cil.varinfo) (v:Regions.t) acc -> 
			let name = Cil.(k.vname) in
			let type_ = Cil.(k.vtype) |> Cil.d_type () |> Pretty.sprint ~width:80 in
			Regions.fold (fun r acc -> Map.add (Region.uniq_of r |> Uniq.to_int) (name, type_) acc) v acc
			) (AFile.global_variables_and_regions file) Map.empty in
		let var_region_map = Cil.(declaration.slocals) |> List.fold_left (fun acc e -> 
			let name = Cil.(e.vname) in
			let type_ = Cil.(e.vtype) |> Cil.d_type () |> Pretty.sprint ~width:80 in
			let regions = AFun.regions_of global_function e in
			let added = Regions.fold (fun r acc -> Map.add (Region.uniq_of r |> Uniq.to_int) (name, type_) acc) regions acc in
			added
			) var_region_map in
		let var_region_map = Map.filter (fun k _ -> k != -1) var_region_map in
        explore_paths path_tree global_function Map.empty var_region_map inline_limit
end