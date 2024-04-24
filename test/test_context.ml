open X86ISTMB

let uniq_ctx_list lst =
  let cmp_fst a b = String.compare (fst a) (fst b) in
  List.map (BatList.unique_cmp ~cmp:cmp_fst) lst

let sort_ctx_list lst = lst |> List.map (BatList.sort compare)

(** [ctx_of_ctx_lst lst] creates a [Context.t] from a list of scopes and
    variable bindings. Scopes that appear earlier in [lst] are guaranteed to be
    pushed before scopes that appear later in [lst]. Key-value pairs that come
    later in each scope will overwrite those that come earlier. *)
let ctx_of_ctx_lst lst =
  let ctx = Context.make () in
  List.iter
    (fun scope ->
      Context.push ctx;
      List.iter (fun (name, value) -> Context.insert ctx name value) scope)
    lst;
  ctx

let gen_ctx_lst =
  QCheck2.Gen.(small_list (small_list (pair string int)) >|= uniq_ctx_list)

let gen_ctx = QCheck2.Gen.(gen_ctx_lst >|= ctx_of_ctx_lst)
let print_ctx_lst = QCheck2.Print.(list (list (pair string int)))

let make_is_empty =
  let test () =
    let open Alcotest in
    let ctx = Context.make () in
    (check bool) "new context is empty" true (Context.is_empty ctx);
    (check int) "new context has zero stack size" 0 (Context.stack_size ctx);
    (check (list (list (pair string int))))
      "new context has no pairs" [] (Context.to_list ctx)
  in
  Alcotest.test_case "empty context properties" `Quick test

let empty_get_none =
  let open QCheck2 in
  let test =
    Test.make ~name:"get on empty always none" ~count:10 Gen.string (fun s ->
        let empty = Context.make () in
        Context.get empty s = None)
  in
  QCheck_alcotest.to_alcotest test

let to_list_correct =
  let open QCheck2 in
  let test =
    Test.make ~name:"to_list correct vars" ~count:100 ~print:print_ctx_lst
      gen_ctx_lst (fun lst ->
        let ctx = ctx_of_ctx_lst lst in
        let result = Context.to_list ctx in
        let expected = lst |> List.rev in
        sort_ctx_list result = sort_ctx_list expected)
  in
  QCheck_alcotest.to_alcotest test

let push_pop_id =
  let open QCheck2 in
  let test =
    Test.make ~name:"push then pop" ~count:100 gen_ctx (fun ctx ->
        let initial = Context.to_list ctx in
        Context.push ctx;
        Context.insert ctx "a" 1;
        Context.pop ctx;
        let final = Context.to_list ctx in
        sort_ctx_list initial = sort_ctx_list final)
  in
  QCheck_alcotest.to_alcotest ~long:true test

let push_stack_size =
  let open QCheck2 in
  let test =
    Test.make ~name:"push stack size" ~count:100 gen_ctx (fun ctx ->
        let initial = Context.stack_size ctx in
        Context.push ctx;
        let final = Context.stack_size ctx in
        final = initial + 1)
  in
  QCheck_alcotest.to_alcotest ~long:true test

let pop_stack_size =
  let open QCheck2 in
  let test =
    Test.make ~name:"pop stack size" ~count:100 gen_ctx (fun ctx ->
        assume (not (Context.is_empty ctx));
        let initial = Context.stack_size ctx in
        Context.pop ctx;
        let final = Context.stack_size ctx in
        final = initial - 1)
  in
  QCheck_alcotest.to_alcotest ~long:true test

let insert_get =
  let open QCheck2 in
  let test =
    Test.make ~name:"insert then get" ~count:100
      QCheck2.Gen.(triple gen_ctx string int)
      (fun (ctx, str, i) ->
        assume (not (Context.is_empty ctx));
        Context.insert ctx str i;
        Context.get ctx str = Some i)
  in
  QCheck_alcotest.to_alcotest ~long:true test

let suite =
  ( "lib/frontend/context.ml",
    [
      make_is_empty;
      empty_get_none;
      to_list_correct;
      push_pop_id;
      push_stack_size;
      pop_stack_size;
      insert_get;
    ] )
