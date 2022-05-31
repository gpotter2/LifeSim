type entity = Entity of Sem.attributVal list

let run env _ init_attrs =
  let lookup_cls x =
    match List.assoc x env with
    | Sem.EntiteClass { attrs; _ } -> attrs
    | _ -> raise (Failure "Bad cls type")
  in
  let instanciate_entity_n entity nb =
    let rec instanciate_entity_n_rec i =
      match i with
      | 0 -> []
      | _ -> entity :: instanciate_entity_n_rec (i - 1)
    in
    instanciate_entity_n_rec nb
  in
  let build_entity attr =
    match attr with
    | name, Sem.Intval nb -> instanciate_entity_n (lookup_cls name) nb
    | _ -> raise (Failure "Bad entity type")
  in
  let rec build_entity_list attrs =
    match attrs with
    | [] -> []
    | x :: rem -> build_entity x @ build_entity_list rem
  in
  let entities = build_entity_list init_attrs in
  let _ =
    List.map
      (fun e -> Sem.printval (Sem.EntiteInst { name = "a"; attrs = e }))
      entities
  in
  ()
