open Owl_plplot
open Owl

(* Graphical config*)
let x_size = 400.0
let y_size = 400.0

(* Amount of instants *)
let instants = 200

(* UTIL FUNCTIONS *)
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

let plot_entities entities i =
  let get_coordinates entity =
    let lookup name attrs =
      match List.assoc name attrs with
      | Sem.Intval x -> Float.of_int x
      | Sem.Floatval x -> x
      | _ ->
          raise (Failure (Printf.sprintf "Attr %s is not an int or float" name))
    in
    match entity with
    | Sem.EntiteInst { attrs; _ } -> (lookup "x" attrs, lookup "y" attrs)
    | _ -> raise (Failure "Bad entity")
  in
  let points_by_entity entity =
    let selected_entities = List.map snd (List.assoc entity entities) in
    let coordinates = List.split (List.map get_coordinates selected_entities) in
    let x, y = (fst coordinates, snd coordinates) in
    let l = List.length selected_entities in
    (Mat.init 1 l (List.nth x), Mat.init 1 l (List.nth y))
  in
  let color i =
    match i mod 5 with
    | 0 -> (223, 22, 22)
    | 1 -> (125, 223, 22)
    | 2 -> (22, 113, 223)
    | 4 -> (225, 168, 4)
    | _ -> (0, 0, 0)
  in
  let plot_entity h i entity =
    let x, y = points_by_entity entity in
    let r, g, b = color i in
    Plot.(
      scatter ~h ~spec:[ Marker "#[0x2295]"; MarkerSize 5.; RGB (r, g, b) ] x y)
  in
  let h = Plot.create (Printf.sprintf "res/%03d.png" i) in
  Plot.set_background_color h 255 255 255;
  Plot.set_xrange h (x_size /. -2.0) (x_size /. 2.0);
  Plot.set_yrange h (y_size /. -2.0) (y_size /. 2.0);
  List.iteri (plot_entity h) (List.map fst entities);
  Plot.output h

(* RUN SIMULATOR FUNCTION *)
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
            let empty =
              match name2 with
              | "always_true" | "none" -> true
              | _ -> (
                  match others with
                  | Sem.Listval [] -> false
                  | _ -> true)
            in
            if not empty then e
            else
              let env =
                match name2 with
                | "none" -> (name1, e) :: environement
                | _ -> (name1, e) :: (name2, others) :: environement
              in
              let r = Sem.eval instruction env in
              match r with
              | Sem.EntiteInst { name; _ } ->
                  if name <> entity1 then
                    raise
                      (Failure
                         (Printf.sprintf
                            "Comportement must return the same entity type ! \
                             (got %s expected %s)"
                            name entity1))
                  else r
              | _ -> raise (Failure "Comportement must return an entity !")
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
                  match entity2 with
                  | "none" -> Sem.Listval []
                  | _ ->
                      Sem.Listval
                        (List.filter_map
                           (fun x -> filter (snd x))
                           (if entity1 = entity2 then
                            List.remove_assoc i entities1
                           else List.assoc entity2 entities))
                in
                (i, apply_comportement e others) :: parc_entities rem
          in
          (entity1, parc_entities entities1)
          :: List.remove_assoc entity1 entities
      | _ -> raise (Failure "Pas un comportement !")
    in
    let rec parc_comportements comps ents =
      match comps with
      | [] -> ents
      | c :: rem -> parc_comportements rem (jouer_comportement c ents)
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
  let _ = Printf.printf "# En cours:\n" in
  let run_all all_ents =
    let rec run_all_rec ents i =
      if i == instants then ()
      else
        let new_ents = next_instant ents in
        let _ = plot_entities new_ents i in
        let _ = Printf.printf (if i mod 20 == 19 then ".\n" else ". %!") in
        run_all_rec new_ents (i + 1)
    in
    run_all_rec all_ents 0
  in
  run_all entities
