open Batteries

open Type
open Abs
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

module MakeT (Monitor: PrinterSpec) = struct

  type step = PathTree.step

  (** step.lenv seems not referentially transparent.  This equality test on
      steps seems to work (ignoring step.lenv) *)
  module StepMap = Map.Make (struct

    type t = PathTree.step

    let compare_kind (k1: PathTree.step_kind) (k2: PathTree.step_kind): int =
      match k1, k2 with
      | Stmt il1, Stmt il2 ->
          Pervasives.compare il1 il2
          (* a gamble: instr can embed expressions, and elsewhere there is a
             custom comparator of expressions.  If out of memory errors reappear
             in map, then consider refining  this to use CilExtra.compareExp. *)
      | Test (tk1, e1), Test (tk2, e2) when tk1 = tk2 ->
          CilExtra.compareExp e1 e2
      | Test (tk1, _), Test (tk2, _) ->
          Pervasives.compare tk1 tk2
      | Goto (la1,lo1), Goto (la2, lo2) when la1 = la2 ->
          Cil.compareLoc lo1 lo2
      | Goto (la1,_), Goto (la2, _) ->
          Pervasives.compare la1 la2
      | Ret eo1, Ret eo2 -> Option.compare ~cmp:CilExtra.compareExp eo1 eo2
      | Stmt _, _ -> -1
      | _, Stmt _ -> +1
      | Test _, _ -> -1
      | _, Test _ -> +1
      | Goto _, _ -> -1
      | _________ -> +1

    let compare (s1: step) (s2: step): int =
      match compare_kind s1.kind s2.kind, Effects.compare s1.effs s2.effs with
      | 0, 0 -> Cil.compareLoc s1.sloc s2.sloc
      | 0, result -> result
      | result, _ -> result

  end)

module RegionMap = Map.Make (Region)

  type 'a set = 'a Set.t
  type 'a region_map = 'a RegionMap.t
  type color = Monitor.state
  type typ = Cil.typ
  type config = color set region_map
  type progress = {
    colors : config StepMap.t ; (* states of pertinent monitors after each step *)
    current: config ;           (* states passed from the previous step *)
    path   : unit -> PathTree.t (* the path that still needs to be explored *)
  }


  let assert_bool = OUnit2.assert_bool;;

  let is_call = function
    | Cil.(Call _) -> true
    | ____________ -> false ;;

  (** Get region from a memory effect, ignore others *)
  let get_region (e: Effects.e): (region * Effects.e) option =
    match e with
    | Effects.Mem (_, region) -> Some (region, e)
    | _______________________ -> None ;;

  let group_by_region (reffs: (region * 'b) list): (region * 'b list) list =
    assert_bool "need non-empty list" (reffs |> List.is_empty |> not) ;
    reffs
      |> List.group (fun r r' -> Region.compare (fst r) (fst r'))
      |> List.map List.split
      |> List.map (Tuple2.map1 List.hd) ;;




  (** Format the file info and the prefix *)
  let format_prefix (file: string) (func: string) : PP.doc =
    PP.(
      words "- func:" ++ !^ func + newline +
      indent (
        !^ "file:" ++ !^ file + newline +
        !^ "lines:"
      )
   )

  let format_colors (region: region) (colors: color set): PP.doc =
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
    |> List.filter (fun d -> d != empty)
    |> comma_sep
    |> brackets ) ;;

  (** Format regions accessed in the step/line *)
  let pp_regions (regions: region list): PP.doc =
    PP.(regions
    |> List.map Region.pp
    |> List.map PP.double_quotes
    |> comma_sep
    |> brackets) ;;

  (** Print a single output line *)
  let pp_step (rt: typ region_map) (step: step) (colors: config): PP.doc =
    let regions = step.effs.may
      |> Effects.EffectSet.to_list
      |> List.filter_map get_region
      |> List.map fst in
    let types = regions
      |> List.filter_map @@ flip RegionMap.find_opt rt
      |> List.map @@ Cil.d_type ()
      |> List.map @@ Pretty.sprint ~width:80
      |> List.map PP.(double_quotes % words)
    in
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
        newline + words "regions:" ++ pp_regions regions +
        newline + words "types:" ++ PP.(types |> comma_sep |> brackets)
      )
    )

  (* TODO: isn't there any other way to compare locations? Why is this here? *)
  let cmp_loc (sc1: step * config) (sc2: step * config): int =
      let s1, s2 = fst sc1, fst sc2 in
      Pervasives.compare s1.sloc.line s2.sloc.line ;;

  (** Print all lines in the provided map *)
  let pp_steps (file: string) (func: string) (rt: typ region_map) (colors: config StepMap.t): PP.doc =
    PP.(colors
      |> StepMap.bindings
      |> List.stable_sort cmp_loc
      |> List.map (uncurry (pp_step rt))
      |> concat
      |> indent
      |> append (format_prefix file func)
      |> flip append @@ newline
    ) ;;



  (** Collect all regions mentioned in [coloring] of all steps, and complete all
      entries for each step with initial states, so that all steps return maps
      (colorings, configs) with the same domain. *)
  let make_total (coloring: config StepMap.t): config StepMap.t =
    let initials = coloring
          |> StepMap.values
          |> Enum.map RegionMap.keys
          |> Enum.map Regions.of_enum
          |> Enum.reduce Regions.union
          |> Regions.enum
          |> Enum.map (fun r -> (r,Set.singleton Monitor.initial_state))
          |> RegionMap.of_enum
    in StepMap.map (RegionMap.union (fun _ _ r -> Some r) initials) coloring ;;


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
    let f _ (s: color set option) (e: Effects.e list option): color set option =
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
      |> group_by_region
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

  (** Create an association list of region ids to variable-type name pairs.
      Used in constructing rvt maps from eba mappings *)
  let rt (v: Cil.varinfo) (rr: Regions.t): (region * typ) Seq.t =
    rr
      |> Regions.to_seq
      |> Seq.map (Tuple2.make v.vtype) |> Seq.map Tuple2.swap ;;




  (** This is the main function of the module. It explores the paths in the
      [file] and prints the coloring for the lines traversed (according to a
      lock monitor). *)
  let print (file: AFile.t) (decl_f: Cil.fundec) (inline_limit: int): unit =
    let _ = Printexc.record_backtrace true in
    let func = AFile.find_fun file decl_f.svar
      |> Option.get
      |> snd in
    let global = AFile.global_variables_and_regions file
      |> Map.to_seq  in
    let local = decl_f.sformals @ decl_f.slocals
      |> Seq.of_list
      |> Seq.map (fun e -> e, AFun.regions_of func e) in
    let rtmap = Seq.append local global
      |> Seq.map (uncurry rt)
      |> Seq.flatten
      |> RegionMap.of_seq in
    let initial = {
      colors  = StepMap.empty ;
      current = RegionMap.empty;
      path    = PathTree.paths_of func } in
    let outcome =
      Printexc.pass (fun _ -> explore_paths func inline_limit initial) () in
    let coloring = make_total outcome.colors in
    let printout = pp_steps decl_f.svar.vdecl.file decl_f.svar.vname rtmap coloring
    in SmartPrint.to_stdout 10000 2 printout ;;

end

module Make (P: PrinterSpec): Printer = MakeT (P)
