let apply_clean_rules_single section i =
  let cur = Asm.Section.get_instr section i in
  match cur with
  | Asm.Instruction.Mov (o1, o2) when o1 = o2 ->
      Asm.Section.rem_instr section i;
      true
  | _ -> false

let apply_clean_rules_pair section i =
  let cur = Asm.Section.get_instr section i in
  let next = Asm.Section.get_instr section (i + 1) in
  let delete_pair () =
    Asm.Section.rem_instr section (i + 1);
    Asm.Section.rem_instr section i
  in
  match (cur, next) with
  | Asm.Instruction.Jmp (Asm.Operand.Label label), Asm.Instruction.Label label2
    when label = Asm.Label.name_of label2 ->
      delete_pair ();
      true
  | (Push op1, Pop op2 | Pop op1, Push op2) when op1 = op2 ->
      delete_pair ();
      true
  | (Add (r1, v1), Sub (r2, v2) | Sub (r1, v1), Add (r2, v2))
    when r1 = r2 && v1 = v2 ->
      delete_pair ();
      true
  | _ -> false

let apply_clean_rules section i =
  let first_try =
    if i > 0 then apply_clean_rules_pair section (i - 1) else false
  in
  let second_try =
    if i < Asm.Section.length_of section then apply_clean_rules_single section i
    else false
  in
  first_try || second_try

let clean_pass section =
  let length = Asm.Section.length_of section in
  let did_change = ref false in
  for rev_i = 0 to length - 1 do
    let i = length - rev_i - 1 in
    did_change := apply_clean_rules section i || !did_change
  done;
  !did_change

let clean section =
  let rec clean_aux section = if clean_pass section then clean_aux section in
  clean_aux section
