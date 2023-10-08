open Dream

let () =
  (* let src = {|
       let foo' = (> 1 2) in
       foo'
     |} in *)
  let src = open_in "./main.dream" in
  let read = Parser.prog Lexer.read (Lexing.from_channel src) in
  close_in src;
  let s =
    (* Ast.show_typ read *)
    (* match read with
       | Some r -> Ast.show_typ r
       | None -> "" *)
    match read with
    | Some r -> Ast.show_expr r
    | None -> ""
  in
  print_newline ();
  print_endline s;
  print_newline ()
