type entity = Entity of Sem.attributVal list

let instants = 100

let print_entities entities =
  List.map
    (fun (name, entities) ->
      let _ = Printf.printf "<%s>:\n" name in
      List.map
        (fun (i, e) ->
          Printf.printf "- %d " i;
          Sem.printval e;
          Printf.printf "\n")
        entities)
    entities

let run env comportements init_attrs =
  let _ = Printf.printf "SIMULATOR\n\n" in
  let lookup_cls x =
    match List.assoc x env with
    | Sem.EntiteClass { name; _ } -> name
    | _ -> raise (Failure "Unknown class !")
  in
  let instanciate_entity_n entity nb =
    let rec instanciate_entity_n_rec i =
      match i with
      | 0 -> []
      | _ ->
          (i, Sem.eval (Ast.EntiteVal (entity, [])) env)
          :: instanciate_entity_n_rec (i - 1)
    in
    instanciate_entity_n_rec nb
  in
  let build_entity attr =
    match attr with
    | name, Sem.Intval nb -> (name, instanciate_entity_n (lookup_cls name) nb)
    | _ -> raise (Failure "Bad entity type")
  in
  let rec build_entity_list attrs =
    match attrs with
    | [] -> []
    | x :: rem -> build_entity x :: build_entity_list rem
  in
  let entities = build_entity_list init_attrs in
  let next_instant entities =
    let jouer_comportement comp entities =
      match comp with
      | Sem.ComportementClass
          {
            entity1;
            entity2;
            name1;
            name2;
            condition;
            instruction;
            environement;
          } ->
          let entities1 = List.assoc entity1 entities in
          let apply_comportement e others =
            let env = (name1, e) :: (name2, others) :: environement in
            match others with
            | Sem.Listval [] -> e
            | Sem.Listval _ -> Sem.eval instruction env
            | _ -> raise (Failure "Impossible case")
          in
          let rec parc_entities ents =
            match ents with
            | [] -> []
            | (i, e) :: rem ->
                let env = (name1, e) :: environement in
                let filter y =
                  match Sem.app (Sem.eval condition env) y with
                  | Sem.Boolval true -> Some y
                  | Sem.Boolval false -> None
                  | _ -> raise (Failure "Condition should return a boolean !")
                in
                let others =
                  Sem.Listval
                    (List.filter_map
                       (fun x -> filter (snd x))
                       (if entity1 == entity2 then List.remove_assoc i entities1
                       else List.assoc entity2 entities))
                in
                (i, apply_comportement e others) :: parc_entities rem
          in
          (entity1, parc_entities entities1)
          :: List.remove_assoc entity1 entities
      | _ -> raise (Failure "Pas un comportement !")
    in
    let rec parc_comportements comps entities =
      match comps with
      | [] -> entities
      | c :: rem -> parc_comportements rem (jouer_comportement c entities)
    in
    parc_comportements comportements entities
  in
  let _ = Printf.printf "# Inital condition:\n" in
  let _ = print_entities entities in
  let _ = Printf.printf "# Comportements:\n" in
  let _ =
    List.map
      (fun x ->
        Printf.printf "- ";
        Sem.printval x;
        Printf.printf "\n")
      comportements
  in
  let _ = next_instant entities in
  ()
