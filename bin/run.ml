let version = "0.01"

open Vol

let usage () =
  let _ =
    Printf.eprintf
      "Usage: %s [file]\n\
       \tLit et exécute un comportement (default is stdin)\n\
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
  let _ =
    Printf.printf "        Simulateur de comportements, version %s\n%!" version
  in
  let global_env = ref [] in
  let init_entities = ref [] in
  let comportements = ref [] in
  let lexbuf = Lexing.from_channel input_channel in
  while true do
    try
      let _ = Printf.printf "> %!" in
      let e = Parse.main Lex.lex lexbuf in
      let _ = Printf.printf "Recognized: " in
      let _ = Ast.print stdout e in
      let _ = Printf.fprintf stdout " =\n%!" in
      let v =
        Sem.eval e
          (("init", Sem.EntiteClass { name = "init"; attrs = !init_entities })
          :: !global_env)
      in
      let _ =
        match v with
        | Sem.EntiteClass { name; _ } ->
            global_env := (name, v) :: !global_env;
            init_entities := (name, Sem.Intval 0) :: !init_entities
        | Sem.ComportementClass { entity1; entity2; _ } -> (
            match
              ( List.assoc_opt entity1 !global_env,
                List.assoc_opt entity2 !global_env )
            with
            | None, None -> Printf.printf "Invalid entity names !\n"
            | _ -> comportements := v :: !comportements)
        | Sem.EntiteInst { name = "init"; attrs } ->
            Simulator.run !global_env !comportements attrs
        | _ -> Sem.printval v
      in
      Printf.printf "\n%!"
    with
    | Lex.Eoi ->
        Printf.printf "Bye bye.\n%!";
        exit 0
    | Failure msg -> Printf.printf "Erreur: %s\n\n" msg
    | Parsing.Parse_error -> Printf.printf "Erreur de syntaxe\n\n"
  done
;;

if !Sys.interactive then () else main ()
