type t =
  | Basic of (Basic_block.t * Liveliness.BasicBlockAnalysis.t -> unit)
  | Combine of t list
  | Repeat of t * int

let make f = Basic f
let sequence pass1 pass2 = Combine [ pass1; pass2 ]
let combine passes = Combine passes
let repeat n pass = Repeat (pass, n)

let execute pass bb liveliness =
  let rec execute_aux bb = function
    | Basic f -> f (bb, liveliness)
    | Combine passes -> List.iter (execute_aux bb) passes
    | Repeat (pass, n) ->
        for _ = 0 to n - 1 do
          execute_aux bb pass
        done
  in
  execute_aux bb pass

module type Sig = sig
  val pass : t
end
