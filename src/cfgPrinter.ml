open Batteries

module L = LazyList

open Type
open Abs
open PathTree
open Effects
open PathTree
open Dolog

module type Printer = sig
	val print : AFile.t -> Cil.fundec -> unit
end

module Make() : Printer = struct
    let rec explore_paths path = 
		let p = path() in
		match p with
		| Seq(step, remaining) -> 
            Format.printf "%s:\n%s " (Utils.Location.pp step.sloc |> PP.to_string) (pp_step step |> PP.to_string);
		    List.iter (fun e -> pp_e e |> PP.to_string |> Format.printf "%s ") (EffectSet.to_list step.effs.may);
            Format.printf "\n\n";
            explore_paths remaining
		| Assume(_, _, remaining) -> 
			explore_paths remaining
		| If(true_path, false_path) -> 
			explore_paths true_path;
            explore_paths false_path;
		| Nil -> ()

	let print file declaration =
		let variable_info = Cil.(declaration.svar) in
        let _, global_function = Option.get(AFile.find_fun file variable_info) in
        let path_tree = paths_of global_function in
        explore_paths path_tree
end

module CFGPrinter = Make()

include CFGPrinter