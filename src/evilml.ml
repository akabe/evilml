(* Evil ML --- A compiler from ML to C++ template language

   Copyright (C) 2015 Akinori ABE <abe@sf.ecei.tohoku.ac.jp>

   Evil ML is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Evil ML is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open EmlUtils
open Format

module Opts =
struct
  let use_dirs = ref []
  let header = ref (Filename.concat EmlConfig.include_dir "evilml.hpp")
  let input_file = ref ""
  let output_file = ref ""
  let verbose = ref false
  let embed = ref false

  let speclist =
    [
      ("-I", Arg.String (fun s -> use_dirs := s :: !use_dirs),
       "\tAdd directory for #use-directive");
      ("--header", Arg.Set_string header, "\tSpecify path of C++ header");
      ("--output", Arg.Set_string output_file, "\tSpecify an output file");
      ("--embed", Arg.Set embed, "\tEmbed header file \"evilml.hpp\"");
      ("--verbose", Arg.Set verbose, "\tVerbose mode");
    ]

  let () =
    let usage_msg =
      "Evil ML is a compier from ML to C++ template language.\n\
       \n\
       Usage: evilml [options] filename\n" in
    Arg.parse speclist (fun s -> input_file := s) usage_msg;
    if !input_file = "" then begin (* Check input filename *)
      Arg.usage speclist usage_msg;
      exit (-1)
    end;
    if !output_file = ""
    then output_file := (Filename.chop_extension !input_file) ^ ".cpp"
end

let header =
  if !Opts.embed
  then sprintf "#line 1 %S\n%s\n#line 1 %S"
      !Opts.header (read_file !Opts.header) !Opts.output_file
  else sprintf "#include %S" !Opts.header

let hook_typing =
  let f tops =
    List.iter (fun top -> match top.EmlLocation.data with
        | EmlTypedExpr.Top_let (_, id, ts, _) ->
          printf "val %s : %a@." id EmlType.pp_scheme ts
        | _ -> ()) tops
  in
  if !Opts.verbose then Some f else None

let loader loc fname =
  try
    let path = !Opts.use_dirs @ [EmlConfig.include_dir; "."]
               |> List.map (fun dir -> Filename.concat dir fname)
               |> List.find Sys.file_exists in
    Lexing.from_channel (open_in path)
  with Not_found ->
    errorf ~loc "File %S is not found" fname ()

let main in_fname out_fname =
  let ic = open_in in_fname in
  let oc = open_out out_fname in
  let ppf = formatter_of_out_channel oc in
  begin
    try
      Lexing.from_channel ic
      |> EmlCompile.run ~loader ?hook_typing ~header in_fname
      |> List.iter (fprintf ppf "%a@\n@\n" EmlCpp.pp_decl)
    with
    | Compile_error ({ EmlLocation.loc; EmlLocation.data; }) ->
      eprintf "%a@\nError: %s@\n@\n[Stack Trace]@." EmlLocation.pp loc data;
      Printexc.print_backtrace stderr
  end;
  pp_print_flush ppf ();
  close_out oc;
  close_in ic

let () = main !Opts.input_file !Opts.output_file
