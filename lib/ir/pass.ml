type t =
  | Basic of (Basic_block.t -> unit)
  | Combine of t list
  | Repeat of t * int

let make f = Basic f
let compose pass1 pass2 = Combine [ pass1; pass2 ]
let combine passes = Combine passes
let repeat n pass = Repeat (pass, n)

let execute pass bb =
  let rec execute_aux bb = function
    | Basic f -> f bb
    | Combine passes -> List.iter (execute_aux bb) passes
    | Repeat (pass, n) ->
        for _ = 0 to n - 1 do
          execute_aux bb pass
        done
  in
  execute_aux bb pass

module type PASS = sig
  val pass : t
end
