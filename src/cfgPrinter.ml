open Batteries

open Type
open Abs
open BatTuple.Tuple2

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

module MakeT (P: PrinterSpec) = struct

  let assert_bool = OUnit2.assert_bool;;

  let lock_functions = [
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


  (* TODO: likely belongs elsewhere. Util? *)
  let is_call (instr: Cil.instr): bool =
    match instr with
    | Cil.(Call _) -> true
    | ____________ -> false ;;


  (* TODO: this is not the right module to define this type, and perhaps
     already something like that exists. But helps for now. Eliminate. *)
  (** maps a region number to a variable name and a type name/string *)
  type rvtmap = (int, name * name) BatMap.t

  (* TODO: this might be eliminatable *)
  (** Get region from a memory effect, ignore others *)
  let get_region e =
    match e with
    | Effects.Mem (_, region) -> Some (region, e)
    | _______________________ -> None;;

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
  (** Translate a region [r] and state [s] information into a log entry
      containing state, the lock name, the variable type and the region name,
      plus all the regions involved in the calls *)
  let region_state_string (r: region) (s: P.state) (m: rvtmap): string =
    let r_string = Region.pp r |> PP.to_string in
    let _ = assert_bool (Printf.sprintf "Region r is bound %s" r_string)
      (Region.is_meta r) in
    let _ = assert_bool "region info undefined in var-region map!"
      (BatMap.mem (region_id r) m) in
    let sname = P.string_of_state s in
    let vname, vtype = rvtmap_get m (region_id r) in
      Printf.sprintf "%s, LockName:%s, LockType:%s, LockRegion:%s"
        sname vname vtype r_string ;;


  (** Print a file location including a function name *)
  let loc_prefix (l: Cil.location) (fname: string): PP.doc =
    PP.(Utils.Location.pp l + colon + (!^ fname) + colon)


  let cil_tmp_dir = Hashtbl.create 50


  (* TODO: it might be beneficial to use the PP and return it - this should
      make the printer much faster, and pure *)
  (* TODO: not sure if the mutual recursion can be eliminated, the stack may be
     needed. The refactoring of this function is not finished, as I decided that
     do_step has bigger benefits to gain - and also the coroutine structure can
     be recovered from there - which would make this one easier to refactor. *)
  let rec explore_paths  (func: AFun.t) (map: (region, P.state list) Map.t)
    (rvtmap: rvtmap) (inline_limit: int) (path: unit -> PathTree.t): unit =
    match path () with
    | Seq (step, remaining) ->
        begin
          match do_step step func map rvtmap inline_limit with
          | Some (without_monitors_in_final_states, rvtmap) ->
              explore_paths func without_monitors_in_final_states rvtmap inline_limit remaining
          | None -> ()
        end
    | Assume (_, _, remaining) ->
       explore_paths func map rvtmap inline_limit remaining
    | If (true_path, false_path) ->
       explore_paths func map rvtmap inline_limit true_path;
       explore_paths func map rvtmap inline_limit false_path
    | Nil -> ()


  and do_step (step: PathTree.step) (func: AFun.t)
    (rsmap: (region, P.state list) Map.t) (rvtmap: rvtmap) (inline_limit: int) =

    let _ = if inline_limit > 0 then Option.(
      Some step
        |> filter (PathTree.exists_in_stmt is_call)
        |> (flip bind) (PathTree.inline func)
        |> may (ignore % explore_paths func rsmap rvtmap (inline_limit - 1) % snd)
    ) in (* TODO: eventually should produce a PP.doc, this is why it remains a let *)
         (* TODO: it also seems that we ignore a failure if inline fails, but does it? *)

    let apply_transition effects map_to_add_to =
      List.fold_right (fun ((r,es): region * Effects.e list) map ->
          let states  = Map.find_default [P.initial_state] r map_to_add_to in
          let applied = List.map (fun s -> P.transition s es) states in
          Map.add r applied map) effects map_to_add_to in
    begin

    let input = step.effs.may |> Effects.EffectSet.to_list in
    let region_options = List.map get_region input in
    let regions = List.fold_right (fun e acc -> (match e with Some r -> r::acc | None -> acc)) region_options [] in
    let grouped = List.group (fun r r' -> Region.compare (fst r) (fst r')) regions |> List.map extract_regions in
    let states = apply_transition grouped rsmap in
    let interesting_monitors = Map.filter (fun _ b -> List.exists (fun s -> P.is_in_interesting_section s) b) states in
      if not (Map.is_empty interesting_monitors)
      then
        begin
          (* let ints = enum_regions step.effs |> List.of_enum |> List.map region_id in *)

          (* Gets the name of the expression involving a call,
           it is for function name or arguments
           although it works over expressions *)
          let rec getFAname (exp : Cil.exp) : string =
            let rec explore_offset (offs:Cil.offset) = match offs with
              |NoOffset -> ""
              |Field (info, NoOffset) -> info.fname
              |Field (info, o) -> info.fname ^"."^(explore_offset o)
              |Index (e,o) -> (getFAname e)^"[i]"^(explore_offset o) in
            match exp with
            |Lval (Var name,o) ->
              let offr = explore_offset o in
              if offr <> "" then name.vname ^"."^offr else
                name.vname
            |Lval (Mem e, o) ->
              let offr = explore_offset o in
              if offr <> "" then (getFAname e)^"."^offr else
                (getFAname e)
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
            |_ -> "Unknown"
          in
          let fcall = match step.kind with
            | Stmt l -> List.filter (fun (i:Cil.instr) -> match i with Call _ -> true | _ -> false) l
            | _ -> []
          in
          (* look calls that manipulate locks *)
          let lcall = List.filter is_locking fcall in
          (* Keep track of function calls whos results are assigned to cil tmp variables *)
          List.iter(fun (i:Cil.instr) ->
              match i with
              |Call (Some (Cil.Var vi,_),_,arg1::_args,_) ->
                if BatString.starts_with vi.vname "tmp" then
                  begin
                    (*Printf.eprintf "Tmp assignment: %s -> %s\n" vi.vname (getFAname arg1);*)
                    Hashtbl.remove cil_tmp_dir vi.vname;
                    Hashtbl.add cil_tmp_dir vi.vname (getFAname arg1);
                  end
              |_ -> ())fcall;
          let lnames = List.map (fun (fc:Cil.instr) ->
                           match fc with
                           |Call(_,_,args,_) -> getFAname (List.hd args)
                           |_ -> "") lcall in

          let (lname:string) = List.fold_left (fun x acc -> if acc == "" then x else acc) "" lnames in
          let rvtmap =
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
                    List.exists (fun s -> P.string_of_state s = "Locked" ||
                                            P.string_of_state s = "Unlocked") v
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
          in


          (* TODO: the "" below should be replaced with function name, or we should refactor it away *)
          Printf.printf "%s:%s\n"
            (loc_prefix step.sloc "" |> PP.to_string) (PathTree.pp_step step |> PP.to_string);
          (Map.iter (fun k v ->
               List.iter (fun s ->
                   if (P.string_of_state s ="Locked")||(P.string_of_state s ="Unlocked")
                   then
                       Printf.fprintf IO.stdout "{State:%s}" (region_state_string k s rvtmap)
                 )v
             )interesting_monitors;
          Printf.fprintf IO.stdout "\n");

          List.iter (fun e ->
              Effects.pp_e e |> PP.to_string |> Printf.fprintf IO.stdout "{Effect:%s}";
              let region = get_region e in
              match region with
             | Some r ->
                let id = region_id (fst r) in
                Printf.fprintf IO.stdout "{Region:%i} " id;
                rvtmap_apply id rvtmap (Printf.fprintf IO.stdout "{Reference:{Vartype:%s}{Varname:%s}}");
             | None -> ();
                Printf.fprintf IO.stdout "\n";
          ) (Effects.EffectSet.to_list step.effs.may);
          Printf.fprintf IO.stdout "\n";

       let without_monitors_in_final_states =
         Map.map (fun state_list -> List.filter (fun s -> not (P.is_in_final_state s)) state_list) states in
         Some (without_monitors_in_final_states, rvtmap)
            (* the original recursion: explore_paths func without_monitors_in_final_states
               rvtmap inline_limit remaining *)
    end else None
    end ;;



  (** This is the main function of the module. It explores the paths in the
      [file] and prints the coloring for the lines traversed (according to a
      lock monitor). *)
  let print (file: AFile.t) (decl_f: Cil.fundec) (inline_limit: int): unit =
    let func = AFile.find_fun file decl_f.svar |> Option.get |> snd in
    let global = AFile.global_variables_and_regions file |> Map.to_seq in
    let local = decl_f.sformals @ decl_f.slocals |> Seq.of_list
      |> Seq.map (fun e -> e, AFun.regions_of func e) in
    let rvtseq = Seq.append local global |> Seq.map (uncurry rvt_mk) |> Seq.flatten in
    let rvtmap = Map.of_seq rvtseq in
    let path_tree = PathTree.paths_of func in
      (* TODO: printed prematurely, kept here for backwards traceability *)
      loc_prefix decl_f.svar.vdecl decl_f.svar.vname |> PP.to_stdout;
      assert_bool "Duplicate regions!" (Map.cardinal rvtmap = Seq.length rvtseq);
      assert_bool "Regions with id -1!" (Map.for_all (fun k _ -> k != -1) rvtmap);
      explore_paths func Map.empty rvtmap inline_limit path_tree;;

end

module Make (P: PrinterSpec): Printer = MakeT (P)
