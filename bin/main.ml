open Dream

let () =
  let src = open_in "./main.dream" in
  let read = Parser.prog2 Lexer.read (Lexing.from_channel src) in
  close_in src;
  let s =
    match read with
    | Some r -> Ast.show_program r
    | None -> ""
  in
  print_newline ();
  print_endline s;
  print_newline ()
