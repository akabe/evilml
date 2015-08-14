open Format
open Utils

let set_lexing lexbuf fname =
  let open Lexing in
  lexbuf.lex_curr_p <- { pos_fname = fname; pos_lnum = 1;
                         pos_bol = 0; pos_cnum = 0; }

let print_type top = match top.Location.data with
  | TypedExpr.Top_let (_, id, ts, _) ->
    printf "val %s :: %a@." id Type.pp_scheme ts
  | _ -> ()

let main fname =
  let ic = open_in (fname ^ ".ml") in
  let oc = open_out (fname ^ ".cpp") in
  let ppf = formatter_of_out_channel oc in
  let lexbuf = Lexing.from_channel ic in
  set_lexing lexbuf (fname ^ ".ml");
  begin
    try
      lexbuf
      |> Parser.main Lexer.main (* parsing *)
      |> Typing.typing []
      (* |> List.iter (printf "%a@."
         (TypedExpr.pp_base_top Typing.pp_ext_expr)) *)
      |> (fun tops ->
          printf "[Typing]@.";
          List.iter print_type tops;
          tops)
      |> RemoveMatch.convert
      |> Alpha.convert
      |> UnCurrying.convert
      |> Assoc.convert
      (* |> List.iter (printf "%a@."
         (TypedExpr.pp_base_top RemoveMatch.pp_ext_expr)) *)
      |> Boxing.convert []
      (* |> List.iter (printf "%a@."
         (TypedExpr.pp_base_top Boxing.pp_ext_expr)) *)
      |> FlatLet.convert
      (* |> List.iter (printf "%a@." FlatLet.pp_top) *)
      |> Cpp.convert
      |> List.iter (fprintf ppf "%a@\n@\n" Cpp.pp_decl)
    with
    | Compile_error ({ Location.loc; Location.data; }) ->
      eprintf "%a@\nError: %s@\n@\n[Stack Trace]@." Location.pp loc data;
      Printexc.print_backtrace stderr
  end;
  pp_print_flush ppf ();
  close_out oc;
  close_in ic

let () =
  if Array.length Sys.argv <> 2
  then eprintf "Usage: %s source-file@." Sys.argv.(0)
  else if not (Filename.check_suffix Sys.argv.(1) ".ml")
  then eprintf "Extension should be .ml: %s" Sys.argv.(1)
  else main (Filename.chop_extension Sys.argv.(1))
