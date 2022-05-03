open Vol

let version = "0.01"

let usage () =
  let _ =
    Printf.eprintf
      "Usage: %s [file]\n\
       \tRead a PCF program from file (default is stdin) Compiles PCF into \
       bytecode.\n\
       Takes input from standard input ended by ^D or from a file passed as \
       argument.\n\
      \ Bytecode file is always named \'a.out'.\n\
       %!"
      Sys.argv.(0)
  in
  exit 1

let main () =
  let input_channel =
    match Array.length Sys.argv with
    | 1 -> stdin
    | 2 -> (
        match Sys.argv.(1) with
        | "-" -> stdin
        | name -> (
            try open_in name
            with _ ->
              Printf.eprintf "Opening %s failed\n%!" name;
              exit 1))
    | _ -> usage ()
  in
  let out_channel = open_out_bin "a.out" in
  let compiled_sentences = ref ([] : VmBytecode.vm_code list) in
  let lexbuf = Lexing.from_channel input_channel in
  let _ = Printf.printf "        Welcome to PCF, version %s\n%!" version in
  while true do
    try
      let _ = Printf.printf "> %!" in
      let e = Pcfparse.main Pcflex.lex lexbuf in
      let code = Compile.compile_expr [] e in
      Printf.printf "%a" PrintByteCode.pp_code code;
      (* Stored in reverse order for sake of efficiency. *)
      compiled_sentences := code :: !compiled_sentences;
      Printf.printf "\n%!"
    with
    | Pcflex.Eoi ->
        (* Ok, right, job is done. We still just need to write the bycode
           file. No need to reverse first the list of the compiled sentences,
           List.fold_left will do this for us by the way. *)
        let whole_code =
          List.fold_left (fun accu code -> code @ accu) [] !compiled_sentences
        in
        output_value out_channel whole_code;
        close_out out_channel;
        Printf.printf "Bye.\n%!";
        exit 0
    | Failure msg -> Printf.printf "Error: %s\n\n" msg
    | Parsing.Parse_error -> Printf.printf "Syntax error\n\n"
    | Compile.Unbound_identifier id ->
        Printf.printf "Unbound identifier %s.@." id
    | Compile.Compilation_not_implemented ->
        Printf.printf "Compilation not yet implemented@."
  done
;;

if !Sys.interactive then () else main ()
