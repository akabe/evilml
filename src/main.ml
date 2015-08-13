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

open Format
open Utils

module Opts =
struct
  let input_file = ref ""
  let output_file = ref ""
  let verbose = ref false
  let embed_header = ref false

  let speclist =
    [
      ("--output", Arg.Set_string output_file, "\tSpecify an output file");
      ("--embed", Arg.Set embed_header, "\tEmbed header file \"evilml.hpp\"");
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

let read_file fname =
  let ic = open_in fname in
  let b = Buffer.create 256 in
  try
    while true do
      Buffer.add_string b (input_line ic);
      Buffer.add_char b '\n'
    done;
    assert false
  with End_of_file ->
    close_in ic;
    Buffer.contents b

let make_header () =
  let basename = "evilml.hpp" in
  let cwd = Sys.getcwd () in
  try
    let fname = [cwd; Filename.concat cwd "includes"; Config.datadir]
                |> List.map (fun dir -> Filename.concat dir basename)
                |> List.find Sys.file_exists in
    if !Opts.embed_header
    then read_file fname
    else sprintf "#include %S" fname
  with Not_found -> errorf "Not found %S" basename ()

let main in_fname out_fname =
  let ic = open_in in_fname in
  let oc = open_out out_fname in
  let ppf = formatter_of_out_channel oc in
  begin
    try
      Lexing.from_channel ic
      |> Compile.run ~verbose:!Opts.verbose ~header:(make_header ()) in_fname
      |> List.iter (fprintf ppf "%a@\n@\n" Cpp.pp_decl)
    with
    | Compile_error ({ Location.loc; Location.data; }) ->
      eprintf "%a@\nError: %s@\n@\n[Stack Trace]@." Location.pp loc data;
      Printexc.print_backtrace stderr
  end;
  pp_print_flush ppf ();
  close_out oc;
  close_in ic

let () = main !Opts.input_file !Opts.output_file
