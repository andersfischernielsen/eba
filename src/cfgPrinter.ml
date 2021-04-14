open Batteries

open Type
open Abs
open BatTuple.Tuple2
open Option.Infix


module type PrinterSpec = sig

  type state
  val transition: state -> Effects.e list -> state
  val is_in_interesting_section: state -> bool
  val initial_state: state
  val is_in_transition_labels: Effects.e -> bool
  val is_in_final_state: state -> bool
  val string_of_state: state -> string

end


module type Printer = sig

  val print : AFile.t -> Cil.fundec -> int -> unit

end

(** Lines for which no activity of the monitor is observed are presently not
    printed. So for double lock, for instance, assume that these attracted no
    colors (which should mean black). *)
module MakeT (Monitor: PrinterSpec) = struct



  (* Name several useful types to increase readability *)

  type step = PathTree.step

  (** step.lenv seems to be not stable across several references to the same
      program point.  I am not entirely sure why. But this boolean equality
      test on steps seems to work (just ignoring step.lenv) for detecting
      whether we have seen a step. It might be that elsewhere in EBA this is
      already implemented but I was not able to find it -- TODO *)
  module StepMap = Map.Make (struct

    type step = PathTree.step
    type step_kind = PathTree.step_kind
    type t = step

    let compare_step_kind (k1: step_kind) (k2: step_kind): int =
      match k1, k2 with
      | Stmt il1, Stmt il2 ->
          Pervasives.compare il1 il2
          (* The above seems to be a gamble as instr can embed expressions, and
             for some reason, Iago has built his own comparator of expressions.
             If out of memory errors reappear in map, then perhaps we need to
             refine this to use CilExtra.compareExp. *)
      | Test (tk1,e1), Test (tk2,e2) ->
          if tk1 = tk2
          then CilExtra.compareExp e1 e2
          else Pervasives.compare tk1 tk2
      | Goto (la1,lo1), Goto (la2, lo2) ->
          if la1 = la2
          then Cil.compareLoc lo1 lo2
          else Pervasives.compare la1 la2
      | Ret eo1, Ret eo2 ->
          Option.compare ~cmp:CilExtra.compareExp eo1 eo2
      | Stmt _, _ -> -1
      | _, Stmt _ -> +1
      | Test _, _ -> -1
      | _, Test _ -> +1
      | Goto _, _ -> -1
      | _________ -> +1

    let compare (s1: step) (s2: step): int =
      match compare_step_kind s1.kind s2.kind with
      | 0 ->
          let cmp_effects = Effects.compare s1.effs s2.effs
          in if cmp_effects = 0
          then Cil.compareLoc s1.sloc s2.sloc
          else cmp_effects
      | result -> result
  end)

  module RegionMap = Map.Make (Region)

  (** maps a region number to a variable name and a type name/string *)
  type rvtmap = (int, name * name) BatMap.t

  type 'a set = 'a Set.t
  type 'a region_map = 'a RegionMap.t

  (* TODO: concerned that region_map might still ignore zonking *)
  type config = Monitor.state set region_map

  (* TODO: temporary type to have one place of definition, definitely still messy *)
  type progress = {
    colors : config StepMap.t ; (* states of pertinent monitors after the step *)
    current: config ;
    path   : unit -> PathTree.t
  }


  let assert_bool = OUnit2.assert_bool;;

  let is_call (instr: Cil.instr): bool =
    match instr with
    | Cil.(Call _) -> true
    | ____________ -> false ;;


  (* TODO: this might be eliminatable *)
  (** Get region from a memory effect, ignore others *)
  let get_region (e: Effects.e): (Region.t * Effects.e) option =
    match e with
    | Effects.Mem (_, region) -> Some (region, e)
    | _______________________ -> None ;;

  let ty_name (v: Cil.varinfo): name =
    Cil.d_type () v.vtype |> Pretty.sprint ~width:80;;

  (* TODO: the function name is more specific than type *)
  let extract_regions (r_es: ('a * 'b) list): 'a * 'b list =
    let _ = assert_bool "extract_regions requires a non-empty list"
      (List.is_empty r_es |> not) in
    let split = List.split r_es in
    (split |> fst |> List.hd, snd split)


  (* TODO very likely exists, or should exist elsewhere *)
  (* TODO this function does not return id of an equivalence class *)
  (** Convert a region name [r] to a unique integer identifier for its
      unification class.*)
  let region_id (r: region): int =
    r |> Region.zonk |> Region.uniq_of |> Uniq.to_int


  (* TODO: this might go if we kill rvt maps altogether *)
  (** Create an association list of region ids to variable-type name pairs.
      Used in constructing rvt maps from eba mappings *)
  let rvt_mk (v: Cil.varinfo) (rr: Regions.t): (int * (name * name)) Seq.t =
    Seq.map (fun r -> (region_id r, (v.vname, ty_name v))) (Regions.to_seq rr);;



  (*  TODO appears specific, and possibly exists elsewhere *)
  (** Find the region [r] in the map [map] and apply [func] *)
  let rvtmap_apply (r: int) (m: rvtmap) (f: name -> name -> unit): unit =
    match BatMap.find_opt r m with
    | Some (name, type_) -> f type_ name
    | __________________ -> ()


  (*  TODO does not belong here, and possibly exists elsewhere *)
  (** Get the variable name and type name for region [r] stored in
      the rvtmap [m].  Empty strings if not stored.
      TODO: shouldn't this be an assertion failure instead? *)
  let rvtmap_get (m: rvtmap) (r: int): name * name =
    Option.default ("", "") (BatMap.find_opt r m)


  (*  TODO does not belong here, and possibly exists elsewhere *)
  (** Get the variable name of region [r] stored in the rvtmap [m].
      Empty string if not stored.
      TODO: shouldn't this be an assertion failure instead? *)
  let rvtmap_get_name (m: rvtmap) (r: int): name = rvtmap_get m r |> fst



  (* TODO: a bit too many args? what is calls? *)
  (* TODO: why are calls needed here? *)
  (* TODO: Would it make sense to return doc? *)
  (** Translate a region [r] and state [s] information into a log entry
      containing state, the lock name, the variable type and the region name,
      plus all the regions involved in the calls *)
  let region_state_string (r: region) (s: Monitor.state) (m: rvtmap): string =
    let r_string = Region.pp r |> PP.to_string in
    let _ = assert_bool (Printf.sprintf "Region r is bound %s" r_string)
      (Region.is_meta r) in
    let _ = assert_bool "region info undefined in var-region map!"
      (BatMap.mem (region_id r) m) in
    let sname = Monitor.string_of_state s in
    let vname, vtype = rvtmap_get m (region_id r) in
      Printf.sprintf "%s, LockName:%s, LockType:%s, LockRegion:%s"
        sname vname vtype r_string ;;

  (* TODO: remove once debugging is over *)
  let step_kind_to_string (k: PathTree.step_kind): string =
    match k with
    | Stmt _ -> "Stmt"
    | Test _ -> "Test"
    | Goto _ -> "Goto"
    | Ret _  -> "Ret" ;;


  (** Format the file info and the prefix *)
  let format_prefix (file: string) (func: string) : PP.doc =
    PP.(
      words "- func:" ++ !^ func + newline +
      indent (
        !^ "file:" ++ !^ file + newline +
        !^ "lines:"
      )
   )

  let format_colors (region: region) (colors: Monitor.state set): PP.doc =
    let color_docs = PP.(colors
      |> Set.to_list
      |> List.map (Monitor.string_of_state)
      |> List.map (!^)
      |> comma_sep
      |> brackets
    )
    in PP.(
      newline + !^ "-" ++ double_quotes (Region.pp region) ++ colon ++
      (if Set.is_empty colors then empty else color_docs)
    ) ;;

  (** Format the effects of the step/line *)
  let pp_effects_regions (effects: Effects.EffectSet.t): PP.doc =
    PP.(effects
    |> Effects.EffectSet.to_list
    |> List.map Effects.pp_e
    |> List.map PP.double_quotes
    |> comma_sep
    |> brackets ) ;;

  let pp_effect_name = function
    | Effects.Mem(k,r) -> PP.(Effects.pp_kind k)
    | Effects.Noret    -> PP.(!^ "noret")
    | Effects.IrqsOn   -> PP.(!^ "irqson")
    | Effects.IrqsOff  -> PP.(!^ "irqsoff")
    | Effects.BhsOn    -> PP.(!^ "bhson")
    | Effects.BhsOff   -> PP.(!^ "bhsoff")
    | Effects.Sleep    -> PP.(!^ "sleep")
    | _    -> PP.empty

  (** Format the effects of the step/line *)
  let pp_effects (effects: Effects.EffectSet.t): PP.doc =
    PP.(effects
    |> Effects.EffectSet.to_list
    |> List.map pp_effect_name
    |> List.map PP.double_quotes
    |> comma_sep
    |> brackets ) ;;

  (** Format regions accessed in the step/line *)
  let pp_regions (effects: Effects.EffectSet.t): PP.doc =
    PP.(effects
    |> Effects.EffectSet.to_list
    |> List.filter_map get_region
    |> List.map fst
    |> List.map Region.pp
    |> List.map PP.double_quotes
    |> comma_sep
    |> brackets) ;;

  (** Print a single output line *)
  let format_step (step: step) (colors: config): PP.doc =
    PP.(
      newline + words "- line:" ++ int step.sloc.line + newline +
      indent (
        words "source: |-" + newline +
        indent (PathTree.pp_step step) +
        (if RegionMap.is_empty colors then empty
        else newline + !^ "colors:" + (
          colors
          |> RegionMap.bindings
          |> List.sort (fun a b -> Region.compare (fst a) (fst b))
          |> List.map (uncurry format_colors)
          |> concat
        )) +
        newline + words "effects:" ++ pp_effects_regions step.effs.may +
        newline + words "effects_names:" ++ pp_effects step.effs.may +
        newline + words "regions:" ++ pp_regions step.effs.may
      )
    )

  let cmp_loc (sc1: step * config) (sc2: step * config): int =
      let s1, s2 = fst sc1, fst sc2 in
      Pervasives.compare s1.sloc.line s2.sloc.line ;;

  (** Print all lines in the provided map *)
  let format_steps (file: string) (func: string) (colors: config StepMap.t): PP.doc =
    PP.(colors
      |> StepMap.bindings
      |> List.stable_sort cmp_loc
      |> List.map (uncurry format_step)
      |> concat
      |> indent
      |> append (format_prefix file func)
      |> flip append @@ newline
    ) ;;


  (* TODO: remove *)
  let print_rsmap1 (region: region) (states: Monitor.state set): PP.doc =
    PP.(Region.pp region ++ !^ "->" ++
        brackets (states
        |> Set.to_list
        |> List.map (fun s -> !^ (Monitor.string_of_state s))
        |> comma_sep) );;

  (* TODO: remove *)
  let format_config (rsmap: config): PP.doc =
    PP.( rsmap
        |> RegionMap.bindings
          |> List.map (uncurry print_rsmap1)
          |> space_sep
          ) ;;

  (* TODO: remove? *)
  let pp_printer_state (step: step) (progress: progress) successors colors_post: PP.doc =
    PP.(
      !^ "****** l" + int step.sloc.line + !^ ": "
      + PathTree.pp_step step + newline
      + !^ "- current:" + format_config progress.current + newline
      + !^ "- successors:" + format_config successors + newline
      + !^ "- colors pre:" + format_config (StepMap.find_opt step progress.colors |? RegionMap.empty) + newline
      + !^ "- colors post:" + format_config (StepMap.find_opt step colors_post |? RegionMap.empty) + newline
    ) ;;

  let conf_diff (proposed: config) (seen: config): config =
    let diff _ proposed seen =
      match proposed, seen with
      | _, None | None, _ -> proposed
      | Some p, Some s -> Some (Set.diff p s) in
    RegionMap.merge diff proposed seen ;;


  (** Merge the old and new set of colors for a region by unioning them. Do this
      for any region that is mentioned in either new or the old map. *)
  let conf_union (proposal: config) (seen: config): config =
    RegionMap.union (fun _ s1 s2 -> Some (Set.union s1 s2)) proposal seen ;;


  let add_colors (step: step) (to_visit: config)
    (visited: config StepMap.t): config StepMap.t =
    StepMap.modify_def RegionMap.empty step (conf_union to_visit) visited


  (** Execute all monitor automata in the configuration [current] by letting
      them see all the effects in the map [effects].  Both structures are
      indexed by regions, and the operation is point-wise. Produces a successor
      configuration for a step (which is the coloring used in the next step!).
   *)
  let fire_transitions (current: config) (effects: Effects.e list region_map)
    : config =
    let f _ (s: Monitor.state set option) (e: Effects.e list option)
      : Monitor.state set option =
      match s, e with
      | Some states, Some effects ->
        Some (Set.map (flip Monitor.transition effects) states)
      | Some states, None ->
        Some (Set.map (flip Monitor.transition []) states)
      | None, Some effects ->
        if List.exists Monitor.is_in_transition_labels effects
        then Some (Set.singleton
          @@ Monitor.transition Monitor.initial_state effects)
        else None
      | _ -> None (* shouldn't happen *)
    in RegionMap.merge f current effects ;;


  (** Explore a step of execution, a CFG edge, without inlining.  This function
      updates all the dictionary tracking data for printing. It only applies the
      step to monitors that have not been applied to this step. Otherwise the
      progress state is not changed.  *)
  let step_over (progress: progress) (step: step): progress =
    let successors = step.effs.may
      |> Effects.EffectSet.filter Monitor.is_in_transition_labels
      |> Effects.EffectSet.to_list
      |> List.filter_map get_region
      |> List.group (fun r r' -> Region.compare (fst r) (fst r'))
      |> List.map extract_regions
      |> Seq.of_list
      |> RegionMap.of_seq
      |> fire_transitions progress.current in
    let colors1 = add_colors step successors progress.colors in
    (*let _ = pp_printer_state step progress successors colors1 |> PP.to_stdout in*)
    (*let _ = BatIO.flush_all () in *)
    {
      current = successors ;
      colors = colors1;
      path = progress.path
    } ;;


  (** Explore all reachable paths from the current state of exploration,
      captured by [progress]. *)
  let rec explore_paths (func: AFun.t) (inline_limit: int) (progress: progress)
    : progress =
    match progress.path () with
    | Seq (step, remaining) ->
      let progress1 = Some step
        |>  Option.filter (fun _ -> inline_limit > 0)
        |>  Option.filter (PathTree.exists_in_stmt is_call)
        >>= PathTree.inline func
        |>  Option.map (fun fp -> explore_paths func (inline_limit - 1)
             { progress with path = snd fp })
        |?  step_over progress step
      in explore_paths func inline_limit { progress1 with path = remaining }

    | Assume (_, _, remaining) ->
      explore_paths func inline_limit { progress with path = remaining }

    | If (true_path, false_path) ->
      let progress1 =
        explore_paths func inline_limit { progress with path = true_path }
      in explore_paths func inline_limit {
        progress with colors = progress1.colors; path = false_path }

    | Nil -> progress
  ;;




  (** This is the main function of the module. It explores the paths in the
      [file] and prints the coloring for the lines traversed (according to a
      lock monitor). *)
  let print (file: AFile.t) (decl_f: Cil.fundec) (inline_limit: int): unit =
    let _ = Printexc.record_backtrace true in
    (* TODO: a lot of the code below is not used right now. Revised when done *)
    let func = AFile.find_fun file decl_f.svar
      |> Option.get
      |> snd in
    let global = AFile.global_variables_and_regions file
      |> Map.to_seq in
    let local = decl_f.sformals @ decl_f.slocals
      |> Seq.of_list
      |> Seq.map (fun e -> e, AFun.regions_of func e) in
    let rvtseq = Seq.append local global
      |> Seq.map (uncurry rvt_mk)
      |> Seq.flatten in
    let rvtmap = Map.of_seq rvtseq in
    let initial = {
      colors  = StepMap.empty ;
      current = RegionMap.empty;
      path    = PathTree.paths_of func } in
    let outcome =
      Printexc.pass (fun _ -> explore_paths func inline_limit initial) () in
    let printout =
      format_steps decl_f.svar.vdecl.file decl_f.svar.vname outcome.colors
    in (* TODO: this might be failing when we have aliases *)
      (*assert_bool "Duplicate regions!" (Map.cardinal rvtmap = Seq.length rvtseq);*)
      assert_bool "Regions with id -1!" (Map.for_all (fun k _ -> k != -1) rvtmap);
      SmartPrint.to_stdout 10000 2 printout ;;

end

module Make (P: PrinterSpec): Printer = MakeT (P)

  (* let lock_functions = [
    "mutex_lock";
    "mutex_lock_nested";
    "mutex_lock_interruptible_nested";
    "_spin_lock";
    "_raw_spin_lock";
    "__raw_spin_trylock";
    "_raw_read_lock";
    "_raw_spin_lock_irq";
    "_raw_spin_lock_irqsave";
    "_raw_spin_lock_bh";
    "spin_lock";
    "spin_lock_irqsave";
    "spin_lock_bh";
    "mutex_unlock";
    "_spin_unlock";
    "_raw_spin_unlock";
    "__raw_spin_unlock";
    "_raw_read_unlock";
    "__raw_read_unlock";
    "_raw_spin_unlock_irq";
    "__raw_spin_unlock_irq";
    "_raw_spin_unlock_irqrestore";
    "_raw_spin_unlock_bh";
    "spin_unlock_irqrestore";
    "spin_unlock";
    "spin_unlock_irqrestore";
    "spin_unlock_bh";
  ];;

  (* TODO: likely belongs elsewhere. The monitor? *)
  let is_locking (i: Cil.instr): bool =
    match i with
    | Call (_, Lval (Var name, _), _, _) ->
        List.exists ((=) name.vname) lock_functions
    | __________________________________ -> false ;;
*)

  (* TODO: bcr code, likely not needed, probably to be removed, but kept here,
     if we discover that we need it, for some time. *)
  (* let rec getFAname (exp : Cil.exp) : string =
    let rec explore_offset (offs:Cil.offset) = match offs with
      |NoOffset -> ""
      |Field (info, NoOffset) -> info.fname
      |Field (info, o) -> info.fname ^"."^(explore_offset o)
      |Index (e,o) -> (getFAname e)^"[i]"^(explore_offset o) in
    match exp with
    |Lval (Var name,o) -> let offr = explore_offset o in if offr <> "" then name.vname ^"."^offr else name.vname
    |Lval (Mem e, o) -> let offr = explore_offset o in if offr <> "" then (getFAname e)^"."^offr else (getFAname e)
    |CastE (_typeOfCast, e) -> getFAname e (* Cast of a variable *)
    |Const (_) -> "const"                  (* Constant *)
    |BinOp (_,e1,e2,_) -> (getFAname e1) ^ "-" ^ (getFAname e2)
    |AddrOf lval -> getFAname (Lval lval)
    |UnOp (_,e,_) -> getFAname e
    |StartOf _ -> "#startOf#"
    |SizeOfE _ -> "#sizeOfE#"
    |SizeOf _ -> "#sizeOf#"
    |AlignOf _ -> "#typeAlign#"
    |AlignOfE _ -> "#typeAlign#"
    |_ -> "Unknown";; *)


  (* TODO This appears to be used by the bcr code, so likely can be replaced by
      another fixpoint structure for our purposes *)
  (* let cil_tmp_dir = Hashtbl.create 50 *)


        (* TODO from bcr
          (* Gets the name of the expression involving a call,
           it is for function name or arguments
           although it works over expressions *)
          let fcall = match step.kind with
            | Stmt l -> List.filter (fun (i:Cil.instr) -> match i with Call _ -> true | _ -> false) l
            | _ -> [] in
          (* TODO: why do we care about this? Aren't effects enough? *)
          (* look calls that manipulate locks *)
          let lcall = List.filter is_locking fcall in
          (* Keep track of function calls whos results are assigned to cil tmp variables *)
            List.iter(fun (i:Cil.instr) ->
                match i with
                | Call (Some (Cil.Var vi,_),_,arg1::_args,_) ->
                  if BatString.starts_with vi.vname "tmp" then
                    begin
                      Hashtbl.remove cil_tmp_dir vi.vname;
                      Hashtbl.add cil_tmp_dir vi.vname (getFAname arg1);
                    end
                |_ -> ())fcall;

          let lnames = List.map (fun (fc:Cil.instr) ->
                           match fc with
                           |Call(_,_,args,_) -> getFAname (List.hd args)
                           |_ -> "") lcall in

          let (lname:string) = List.fold_left (fun x acc -> if acc == "" then x else acc) "" lnames in *)
          (* TODO: this part of the code appears to be important. it does
             something with temparary names. We may need to revive this. *)
          (* let rvtmap =
            if lname <> "" then
              let lname =
                if BatString.starts_with lname "tmp" then
                  try
                    let l_ref = ref "tmp" in
                    while BatString.starts_with !l_ref "tmp" do
                      l_ref := Hashtbl.find cil_tmp_dir lname
                    done;
                    Printf.eprintf "%s\n" !l_ref;
                    !l_ref
                  with Not_found ->
                    Printf.eprintf "Warning: tmp lock name not found in directory\n"; lname
                else
                  begin
                    Printf.eprintf "%s\n" lname;
                    lname
                  end
              in

              let c_regions =
                Map.filter (fun _k v ->
                    List.exists (fun s -> Monitor.string_of_state s = "Locked" ||
                                            Monitor.string_of_state s = "Unlocked") v
                  ) interesting_monitors in
              Map.foldi (fun k _v acc ->
                  try
                    let (vn,vt) = BatMap.find (region_id k) rvtmap in
                    if vn = lname then raise Not_found; (* Just set the lock name once *)
                    ignore(BatString.find lname vn);
                    Printf.eprintf "Lock name set for region %d --> %s\n" (region_id k) lname;
                    BatMap.add (region_id k) (lname,vt) acc
                  with Not_found -> acc
                ) c_regions rvtmap
            else
              rvtmap
          in *)


          (* TODO: Color printing code, should be done outside *)
          (* TODO: the "" below should be replaced with function name, or we should refactor it away *)
          (* PP.(loc_prefix step.sloc "" + colon + PathTree.pp_step step + newline |> to_stdout);
          (Map.iter (fun k v ->
               List.iter (fun s ->
                   if (Monitor.string_of_state s ="Locked")||(Monitor.string_of_state s ="Unlocked")
                   then
                       Printf.printf "{State:%s}" (region_state_string k s rvtmap)
                 )v
             )interesting_monitors;
          Printf.printf "\n"); *)



  (* TODO Effect printing code, should be done, once the exploration is over.*)

  (* List.iter (fun e ->
      Effects.pp_e e |> PP.to_string |> Printf.fprintf IO.stdout "{Effect:%s}";
      let region = get_region e in
      match region with
     | Some r ->
        let id = region_id (fst r) in
        Printf.fprintf IO.stdout "{Region:%i} " id;
        rvtmap_apply id rvtmap (Printf.fprintf IO.stdout "{Reference:{Vartype:%s}{Varname:%s}}");
     | None -> ();
        Printf.printf "\n";
  ) (Effects.EffectSet.to_list step.effs.may);
  Printf.fprintf IO.stdout "\n"; *)



