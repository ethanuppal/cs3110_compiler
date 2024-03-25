{
  open Parser
}

rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf }
  | ""