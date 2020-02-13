(* A checker based on the CTL query: p1 EU (q1 && X(p2 EU q2))
 * or, in English, "q1 after repeatedly p1; and q2 after repeatedly p2"
 *)

open Batteries
open Dolog

module Opts = Opts.Get

open Type
open Abs
open PathTree

module type AutomataChecker = sig
	(** A name to identify the checker *)
	val name : string

	(** Checker's internal state, eg. memory regions to track. *)
	type st

	(** States *)
	type state
	type accepting_state

	(** Selects initial contexts *)
	val select : AFile.t -> Cil.fundec -> shape scheme -> AFun.t -> st L.t
	(** Flags steps of interest for triaging. *)
	val trace : st -> Effects.t -> bool
	(** Test *)
	val transition : st -> step -> st option

	(** Bug data *)
	type bug
	val bug_of_st : st -> bug
	val doc_of_report : fn:Cil.varinfo -> bug -> loc1:Cil.location -> loc2:Cil.location -> trace:path -> PP.doc
end

module type S = sig
	val in_func : AFile.t -> Cil.fundec -> string L.t
end

module Make (A : AutomataChecker) : S = struct
	type report = {
		fn    : Cil.varinfo;
		bug   : A.bug;
		loc1  : Cil.location;
		loc2  : Cil.location;
		trace : path;
	}

	let string_of_report {fn; bug; loc1; loc2; trace} = PP.to_string(
		A.doc_of_report fn bug loc1 loc2 trace
	)

	let same_loc (_,s1,_,_) (_,s2,_,_) = Cil.compareLoc s1.sloc s2.sloc = 0

	let cmp_match (_,s1,p1,_) (_,s2,p2,_) :int =
		let l1 = s1.sloc in
		let l2 = s2.sloc in
		let lc = Cil.compareLoc l1 l2 in
		(* Order reversed so that L.unique_eq will take the simplest match *)
		if lc = 0
		then
			let pc = Int.compare (List.length p1) (List.length p2) in
			if pc = 0
			then -(compare p1 p2)
			else -pc
		else -lc

	(* Remove redundant traces keeping the shortest one (wrt [cmp_match]). *)
	let nodup = L.(unique_eq ~eq:same_loc % (sort ~cmp:cmp_match))

	let search fnAbs fd pt state =
		(**	
			TODO: Implement `search`.
		
			- Somehow explore CFG/trace and generate automata for the control flow paths? 
			- Implement adaption of reachable function to return CFG path for generation? 
			- Find product of CFG and checker automata? 
			- Return `Some state` if accepting state is reached in product? Iterate instead? 
		*)

		let product initials transitions input = 
    		match initials, transitions with 
    		| (a, a2), (t1, t2) -> (t1 input a, t2 input a2)
		in

		let partial_product = product (A.initial, CFG.initial) (A.transition, CFG.transition)
		in

		let result = partial_product state in

		match result with 
		| (_, A.accepting_state) 	-> Some state
		| (_, _) 				 	-> None

	let in_func fileAbs fd =
		let fn = Cil.(fd.svar) in
		let fsch, fnAbs = Option.get(AFile.find_fun fileAbs fn) in
		let seeds = A.select fileAbs fd fsch fnAbs in
		let pt = paths_of fnAbs in
		
		(* TODO: Convert to fold to apply previous state along with input? *)
		let bugs = seeds |> L.map (search fnAbs fd pt) |> L.concat in
		
		bugs |> L.map string_of_report
end
