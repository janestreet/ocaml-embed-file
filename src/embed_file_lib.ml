open! Core
open! Async_kernel
open! Async_unix

let variable_name_of_file_name s =
  String.to_list s
  |> List.mapi ~f:(fun i c ->
    match c with
    | '0' .. '9' -> if i = 0 then sprintf "_%c" c else String.of_char c
    | 'A' .. 'Z' -> String.of_char (Char.lowercase c)
    | 'a' .. 'z' | '_' -> String.of_char c
    | '.' -> "_dot_"
    | '-' -> "_"
    | _ -> sprintf "_0x%x_" (Char.to_int c))
  |> String.concat
;;

let chunk_len = 80

let chunks str =
  let n = String.length str in
  let q = n / chunk_len in
  let r = n % chunk_len in
  let chunk i ~len = String.sub str ~pos:(chunk_len * i) ~len in
  List.concat
    [ List.init q ~f:(fun i -> chunk i ~len:chunk_len)
    ; (if r = 0 then [] else [ chunk q ~len:r ])
    ]
;;

(* We obfuscate "CR" in generated string constants to prevent them from ever triggering
   code review. *)
let replace_CRs : string -> string =
  (* [replace_CRs input] replaces all occurrences in [input] of "CR" with "C\082". *)
  let search_pattern = lazy (String.Search_pattern.create "CR") in
  let replacement = lazy (sprintf "C\\%03d" (Char.to_int 'R')) in
  fun input ->
    String.Search_pattern.replace_all
      (force search_pattern)
      ~in_:input
      ~with_:(force replacement)
;;

let write_ml w ~var ~contents =
  Writer.writef w "let %s =\n  \"" var;
  List.iteri (chunks contents) ~f:(fun i chunk ->
    let escaped_chunk = replace_CRs (String.escaped chunk) in
    if i = 0
    then Writer.writef w "%s" escaped_chunk
    else if String.length chunk > 0 && Char.( = ) chunk.[0] ' '
    then Writer.writef w "\\\n  \\%s" escaped_chunk
    else Writer.writef w "\\\n   %s" escaped_chunk);
  Writer.write w "\"\n;;\n"
;;

let write_alist_ml w ~files =
  Writer.write_line w "\nlet by_filename =";
  let line_start = ref "[" in
  Map.iteri files ~f:(fun ~key:file ~data:var ->
    Writer.writef w "  %s \"%s\", %s\n" !line_start file var;
    line_start := ";");
  Writer.write_line w "  ]";
  Writer.write_line w ";;"
;;

let write_mli w ~var = Writer.writef w "val %s : string\n" var

let write_alist_mli w =
  Writer.write_line
    w
    "\n\
     (** an association list mapping the embedded file basenames to their string values \
     *)";
  Writer.write_line w "val by_filename : (string * string) list"
;;

(* a wrapper around [Writer.with_file] that knows how to use a styler to transform what
   gets written to the file. *)
let with_file filename ~styler ~f =
  Writer.with_file_atomic filename ~f:(fun w ->
    match styler with
    | None -> f w
    | Some styler_path ->
      let%bind styler_proc =
        Process.create
          ~prog:styler_path
          ~args:[ "-"; "-original-file"; filename; "-directory-config"; "jbuild" ]
          ()
        >>| ok_exn
      in
      let styler_complete =
        let%bind () = Writer.splice ~from:(Process.stdout styler_proc) w
        and stderr = Process.stderr styler_proc |> Reader.contents
        and exit_status = Process.wait styler_proc in
        match exit_status with
        | Ok () -> return ()
        | Error status ->
          Error.raise_s
            [%message
              "styler failed" (status : Unix.Exit_or_signal.error) (stderr : string)]
      in
      let styler_stdin = Process.stdin styler_proc in
      let%bind () = f styler_stdin in
      let%bind () = Writer.close styler_stdin in
      styler_complete)
;;

let command =
  Async_command.async
    ~summary:"embed text files as ocaml strings"
    ~readme:(fun () ->
      {|
Generates ocaml code defining string constants containing the contents of the provided
files.

The argument [-styler] should be a path to the [apply-style] executable, or an executable
with a very similar interface.
|})
    (let open Command.Let_syntax in
     let%map_open module_name =
       flag "output" (required string) ~doc:"NAME name of the generated module"
     and output_directory =
       flag
         "output-dir"
         (optional_with_default "." Filename_unix.arg_type)
         ~doc:"PATH where to put the generated module (default = cwd)"
     and with_alist =
       flag "with-alist" no_arg ~doc:"include an alist of file basename -> file contents"
     and paths = anon (non_empty_sequence_as_list ("FILE" %: string))
     and styler =
       flag "styler" (optional Filename_unix.arg_type) ~doc:"FILE code styler"
     in
     fun () ->
       let open Deferred.Let_syntax in
       (* normalize module name *)
       let module_name =
         module_name
         |> String.tr ~target:'-' ~replacement:'_'
         |> String.lowercase
         |> String.capitalize
       in
       let filename ext = output_directory ^/ String.lowercase module_name ^ "." ^ ext in
       let write ext ~write_file_line ~write_alist =
         with_file (filename ext) ~styler ~f:(fun w ->
           let first_time = ref true in
           let%bind files =
             Deferred.List.map ~how:`Sequential paths ~f:(fun path ->
               if !first_time then first_time := false else Writer.newline w;
               let basename = Filename.basename path in
               let var = variable_name_of_file_name basename in
               let%map () = write_file_line w ~var ~path in
               basename, var)
             >>| String.Map.of_alist_exn
           in
           if with_alist then write_alist w ~files;
           Deferred.unit)
       in
       let%bind () =
         write
           "ml"
           ~write_file_line:(fun w ~var ~path ->
             let%map contents = Reader.file_contents path in
             write_ml w ~var ~contents)
           ~write_alist:(fun w ~files -> write_alist_ml w ~files)
       in
       let%bind () =
         write
           "mli"
           ~write_file_line:(fun w ~var ~path:_ ->
             write_mli w ~var;
             Deferred.unit)
           ~write_alist:(fun w ~files:_ -> write_alist_mli w)
       in
       Deferred.unit)
    ~behave_nicely_in_pipeline:false
;;

module Private = struct
  let variable_name_of_file_name = variable_name_of_file_name
end
