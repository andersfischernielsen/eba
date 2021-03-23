open Batteries

open Type
open Abs
open PathTree
open Effects

let assert_msg = Utils.assert_msg


module type PrinterSpec = sig

  type state
  val transition: state -> e list -> state
  val is_in_interesting_section: state -> bool
  val initial_state: state
  val is_in_transition_labels: e -> bool
  val is_in_final_state: state -> bool
  val string_of_state: state -> string

end


module type Printer = sig

  val print : AFile.t -> Cil.fundec -> int -> unit

end

module MakeT (P: PrinterSpec) = struct

  (* TODO: this is not the right module to define this type, and perhaps
     already something like that exists. But helps for now. Eliminate. *)
  type vrmap = (int, name * name) BatMap.t

  (* TODO: this might be eliminatable *)
  (** Get region from a memory effect, ignore others *)
  let get_region e =
    match e with
    | Mem (_, region) -> Some (region, e)
    | _______________ -> None



  (* TODO: the function name is more specific than type *)
  let extract_regions (r_es: ('a * 'b) list): 'a * 'b list =
    let _ = assert_msg 
      ~msg: "extract_regions requires a non-empty list" 
      (List.is_empty r_es |> not) in
    let split = List.split r_es in
    (split |> fst |> List.hd, snd split)


  (*  TODO appears specific, and possibly exists elsewhere *)
  (** Find the region [r] in the map [map] and apply [func] *)
  let vrmap_apply (r: int) (m: vrmap) (f: name -> name -> unit): unit =
    match BatMap.find_opt r m with
    | Some (name, type_) -> f type_ name
    | __________________ -> ()


  (* TODO very likely exists, or should exist elsewhere *)
  (** Convert a region name [r] to a unique integer identifier for its
      unification class.*)
  let region_id r = Region.uniq_of r |> Uniq.to_int

  (* TODO: a bit too many args ? *)
  let state_region_string (region: region) (state: P.state) (map: vrmap) (calls: region list): name =
    let variable_name identifier = match Map.Exceptionless.find identifier map with | Some (name, _type)  -> name | None ->"" in
    (*let variable_name _identifier = lname in*)
    let variable_type identifier = match Map.Exceptionless.find identifier map with | Some (_, _type)  -> _type | None ->"" in
    let calls_ = String.concat ", " (List.map (fun c -> variable_name (region_id c)) calls) in
    Format.sprintf "%s,LockName:%s,LockType:%s,LockRegion:%s,FunCall:%s" (P.string_of_state state) (variable_name(region_id region))
      (variable_type(region_id region)) (Region.pp region |> PP.to_string) calls_

  let cil_tmp_dir = Hashtbl.create 50

  let rec explore_paths path func map var_region_map inline_limit =
    let p = path() in
    match p with
    | Seq(step, remaining) ->
       let apply_transition effects map_to_add_to =
         let result = List.fold_right (fun (r_e:region * e list) map ->
         match r_e with
         | r, es ->
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
         begin
           (* let ints = enum_regions step.effs |> List.of_enum |> List.map region_id in *)
           let lock_funs = ["mutex_lock";"mutex_lock_nested";"mutex_lock_interruptible_nested";
                            "_spin_lock";"_raw_spin_lock";"__raw_spin_trylock";"_raw_read_lock";
                            "_raw_spin_lock_irq";"_raw_spin_lock_irqsave";"_raw_spin_lock_bh";"spin_lock";
                            "spin_lock_irqsave";"spin_lock_bh";"mutex_unlock";"_spin_unlock";"_raw_spin_unlock";"__raw_spin_unlock";
                            "_raw_read_unlock";"__raw_read_unlock";"_raw_spin_unlock_irq";"__raw_spin_unlock_irq";
                            "_raw_spin_unlock_irqrestore";"_raw_spin_unlock_bh";"spin_unlock_irqrestore";"spin_unlock";
                            "spin_unlock_irqrestore";"spin_unlock_bh"] in
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
           (* Just look for call that manipulate locks *)
           let lcall = List.filter (fun (i:Cil.instr) ->
                           match i with
                           |Call (_,e,_,_) ->
                             begin
                               match e with
                               |Lval (Var name,_) -> List.exists ((=)name.vname) lock_funs
                               |_ -> false
                             end
                           |_ -> false) fcall
           in
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
           let var_region_map =
             if lname <> "" then
               let lname = if BatString.starts_with lname "tmp" then
                             try
                               let l_ref = ref "tmp" in
                               while BatString.starts_with !l_ref "tmp" do
                                 l_ref := Hashtbl.find cil_tmp_dir lname
                               done;
                               Printf.eprintf "%s\n" !l_ref;
                               !l_ref
                             with Not_found -> Printf.eprintf "Warning: tmp lock name not found in directory\n"; lname
                           else
                             begin
                             Printf.eprintf "%s\n" lname;
                             lname
                             end
               in

               let c_regions = Map.filter (fun _k v ->
                                   List.exists (fun s -> P.string_of_state s = "Locked" || P.string_of_state s = "Unlocked") v
                                 ) interesting_monitors in
               Map.foldi (fun k _v acc ->
                   try
                     let (vn,vt) = BatMap.find (region_id k) var_region_map in
                     if vn = lname then raise Not_found; (* Just set the lock name once *)
                     ignore(BatString.find lname vn);
                     Printf.eprintf "Lock name set for region %d --> %s\n" (region_id k) lname;
                     BatMap.add (region_id k) (lname,vt) acc
                   with Not_found -> acc
                 ) c_regions var_region_map
             else
               var_region_map
           in

           Printf.fprintf IO.stdout "FileName/LineNum:%s:Statements:%s" (Utils.Location.pp step.sloc |> PP.to_string) (pp_step step |> PP.to_string);
           Printf.fprintf IO.stdout ":endStatements\n";
           let call_regions = List.filter_map (fun e -> match e with Mem(Call, r) -> Some r | _ -> None) input in
           (Map.iter (fun k v ->
                List.iter (fun s ->
                    if (P.string_of_state s ="Locked")||(P.string_of_state s ="Unlocked")
                    then
                        Printf.fprintf IO.stdout "{State:%s}" (state_region_string k s var_region_map call_regions)
                  )v
              )interesting_monitors;
           Printf.fprintf IO.stdout "\n");

           List.iter (fun e ->
               pp_e e |> PP.to_string |> Printf.fprintf IO.stdout "{Effect:%s}";
               let region = get_region e in
               match region with
              | Some r ->
                 let id = region_id (fst r) in
                 Printf.fprintf IO.stdout "{Region:%i} " id;
                 vrmap_apply id var_region_map (Printf.fprintf IO.stdout "{Reference:{Vartype:%s}{Varname:%s}}");
              | None -> ();
                 Printf.fprintf IO.stdout "\n";
           ) (EffectSet.to_list step.effs.may);
           Printf.fprintf IO.stdout "\n";

        let without_monitors_in_final_states = Map.map (fun state_list -> List.filter (fun s -> not (P.is_in_final_state s)) state_list) states in
          explore_paths remaining func without_monitors_in_final_states var_region_map inline_limit
        end
    | Assume(_, _, remaining) ->
       explore_paths remaining func map var_region_map inline_limit
    | If(true_path, false_path) ->
       explore_paths true_path func map var_region_map inline_limit;
       explore_paths false_path func map var_region_map inline_limit
    | Nil -> ()

  let print file declaration inline_limit =
    let variable_info = Cil.(declaration.svar) in
    let _, global_function = Option.get(AFile.find_fun file variable_info) in
    Printf.fprintf IO.stdout "---------\n";
    Printf.fprintf IO.stdout "FileName:%s:FunName:%s:LineNum:%i:\n" variable_info.vdecl.file variable_info.vname  variable_info.vdecl.line;
    let path_tree = paths_of global_function in

    let var_region_map = Map.foldi (fun (k:Cil.varinfo) (v:Regions.t) acc ->
      let name = Cil.(k.vname) in
      let type_ = Cil.(k.vtype) |> Cil.d_type () |> Pretty.sprint ~width:80 in
        Regions.fold (fun r acc -> Map.add (region_id r) (name, type_) acc) v acc
    ) (AFile.global_variables_and_regions file) Map.empty in
    let append l1 l2 =
      let rec loop acc l1 l2 =
        match l1, l2 with
        | [], [] -> List.rev acc
        | [], h :: t -> loop (h :: acc) [] t
        | h :: t, l -> loop (h :: acc) t l
      in
      loop [] l1 l2 in
    let var_region_map = append Cil.(declaration.sformals) Cil.(declaration.slocals) |> List.fold_left (fun acc e ->
                                                                   let name = Cil.(e.vname) in
                                                                   let type_ = Cil.(e.vtype) |> Cil.d_type () |> Pretty.sprint ~width:80 in
                                                                   let regions = AFun.regions_of global_function e in
                                                                   let added = Regions.fold (fun r acc -> Map.add (region_id r) (name, type_) acc) regions acc in
                                                                   added
                                                                 ) var_region_map in
    let var_region_map = Map.filter (fun k _ -> k != -1) var_region_map in
    explore_paths path_tree global_function Map.empty var_region_map inline_limit

end

module Make (P: PrinterSpec): Printer = MakeT (P)
