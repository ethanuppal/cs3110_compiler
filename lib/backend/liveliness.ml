open Util
open Ir

(** A set of IR variables. *)
module VariableSet = Set.Make (Variable)

(** Liveliness analysis of an IR operation. *)
type instr_analysis = {
  mutable live_in : VariableSet.t;
  mutable live_out : VariableSet.t option;
}

module BasicBlockAnalysis = struct
  (** AF: An array of instruction analyses
      `[| { li0; lo0 }; { li1; lo1; }; ... |]` represents the liveliness
      analysis of a basic block. [lin] is the set of variables live before
      instruction [n]. If [lon] is [None] and [n] is not the index of the last
      element in the array, and [li(n+1)] is the set of live variables live
      after instruction [n]; if [n] is the last valid index in the array, then
      then the basic block analyzed is an exit point; otherwise, it is
      [Some lon], and [lon] is that set.

      RI: Let [analysis] be a value of type [t]. Then, if [lon] is [None] for
      some [n], then [n < Array.length analysis - 1]. *)
  type t = instr_analysis Array.t

  let rep_ok analysis =
    if Rep_ok.check && Array.length analysis > 0 then
      analysis
      |> Array.iteri (fun i ia ->
             if i < Array.length analysis - 1 && ia.live_out <> None then
               failwith "rep_ok");
    analysis

  let make bb =
    Array.init (Basic_block.length_of bb) (fun _ ->
        { live_in = VariableSet.empty; live_out = None })
    |> rep_ok

  let live_before_instr analysis index =
    let analysis = rep_ok analysis in
    analysis.(index).live_in

  let live_after_instr analysis index =
    let analysis = rep_ok analysis in
    match analysis.(index).live_out with
    | None ->
        if index = Array.length analysis - 1 then VariableSet.empty
        else analysis.(index + 1).live_in
    | Some live_out -> live_out

  let live_in analysis =
    let analysis = rep_ok analysis in
    live_before_instr analysis 0

  let live_out analysis =
    let analysis = rep_ok analysis in
    live_after_instr analysis (Array.length analysis - 1)

  let to_string analysis =
    let analysis = rep_ok analysis in
    let set_to_string set =
      let elements_string =
        VariableSet.elements set
        |> List.map Variable.to_string
        |> String.concat ", "
      in
      "{" ^ elements_string ^ "}"
    in
    "BasicBlockAnalysis {"
    ^ (Seq.init (Array.length analysis) id
      |> List.of_seq
      |> List.map (fun i ->
             "\n  ir[" ^ string_of_int i ^ "] <=> {live_in = "
             ^ set_to_string (live_before_instr analysis i)
             ^ ", live_out = "
             ^ set_to_string (live_after_instr analysis i)
             ^ "}")
      |> String.concat "")
    ^ "\n}"
end

(** [apply_rules liveliness analysis cfg bb ir ir_index ~is_final] applies
    liveliness rules for instruction [ir] at index [ir_index] in basic block
    [bb], where [bb] is in [cfg] and has associated liveliness analysis
    [analysis = IdMap.find liveliness (Basic_block.id_of bb)], and where
    [is_final] if and only if [ir] is the final instruction in [bb], and returns
    whether any updates were made to liveliness information. *)
let apply_rules liveliness analysis cfg bb ir ir_idx ~is_final =
  let result = ref false in
  let instr_analysis = analysis.(ir_idx) in
  let update_live_out new_live_out =
    let old_live_out = instr_analysis.live_out in
    instr_analysis.live_out <- new_live_out;
    if old_live_out <> new_live_out then result := true
  in
  let update_live_in new_live_in =
    let old_live_in = instr_analysis.live_in in
    instr_analysis.live_in <- new_live_in;
    if old_live_in <> new_live_in then result := true
  in
  let read_var var =
    update_live_in (VariableSet.add var instr_analysis.live_in)
  in
  let read_op op = Operand.var_of_opt op |> Option.map read_var |> ignore in
  let write_var var =
    let incoming_live =
      VariableSet.remove var
        (BasicBlockAnalysis.live_after_instr analysis ir_idx)
    in
    update_live_in (VariableSet.union instr_analysis.live_in incoming_live)
  in
  (if is_final then
     let live_out_of_succ =
       Cfg.out_edges cfg bb
       |> List.fold_left
            (fun acc (bb_succ, _) ->
              let incoming_live_partial =
                IdMap.find liveliness (Basic_block.id_of bb_succ)
                |> BasicBlockAnalysis.live_in
              in
              VariableSet.union acc incoming_live_partial)
            VariableSet.empty
     in
     update_live_out (Some live_out_of_succ));
  (match ir with
  | DebugPrint op -> read_op op
  | Assign (var, op) | Deref (var, op) | Ref (var, op) ->
      write_var var;
      read_op op
  | Add (var, op1, op2) | Sub (var, op1, op2) | TestEqual (var, op1, op2) ->
      write_var var;
      read_op op1;
      read_op op2);
  !result

(** [pass work_list liveliness cfg bb] performs a single pass of liveliness
    analysis on a basic block *)
let pass work_list liveliness cfg bb =
  let result = ref false in
  let analysis = IdMap.find liveliness (Basic_block.id_of bb) in
  let ir_view = Basic_block.as_view bb in
  let ir_count = Basic_block.length_of bb in
  for rev_i = 1 to ir_count do
    let i = ir_count - rev_i in
    result :=
      apply_rules liveliness analysis cfg bb (ArrayView.get ir_view i) i
        ~is_final:(rev_i = 1)
      || !result
  done;
  if !result then
    List.iter (fun (bb, _) -> Queue.add bb work_list) (Cfg.in_edges cfg bb);
  !result

(** [iterate liveliness cfg] performs an iteration of liveliness analysis on
    [cfg], updating partial results in [liveliness], and returning whether any
    changes were made. *)
let iterate liveliness cfg =
  let work_list = Queue.create () in
  List.iter (fun bb -> Queue.add bb work_list) (Cfg.exit_points cfg);
  let result = ref false in
  while not (Queue.is_empty work_list) do
    let top_bb = Queue.take work_list in
    result := pass work_list liveliness cfg top_bb || !result
  done;
  !result

let analysis_of cfg =
  let liveliness = IdMap.create 16 in
  Cfg.iter
    (fun bb ->
      IdMap.add liveliness (Basic_block.id_of bb) (BasicBlockAnalysis.make bb))
    cfg;
  let rec converge () = if iterate liveliness cfg then converge () in
  converge ();
  liveliness
