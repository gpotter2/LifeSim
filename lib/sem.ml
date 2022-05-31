open Ast

type pcfval =
  | Intval of int
  | Boolval of bool
  | Stringval of string
  | Funval of { param : string; body : expr; env : environment }
  | Funrecval of {
      fname : string;
      param : string;
      body : expr;
      env : environment;
    }
  | Tupval of pcfval list
  | EntiteClass of { name : string; attrs : attributVal list }
  | EntiteInst of { name : string; attrs : attributVal list }
  | ComportementClass of {
      entity1 : string;
      name1 : string;
      entity2 : string;
      name2 : string;
      condition : expr;
      instruction : expr;
    }

and attributVal = string * pcfval
and environment = (string * pcfval) list

let rec printval = function
  | Intval n -> Printf.printf "%d" n
  | Boolval b -> Printf.printf "%s" (if b then "true" else "false")
  | Stringval s -> Printf.printf "%S" s
  | Funval _ -> Printf.printf "<fun>"
  | Funrecval _ -> Printf.printf "<fun rec>"
  | Tupval x ->
      let rec tup_iter x =
        match x with
        | [] -> ()
        | [ y ] -> printval y
        | y :: req ->
            let _ = printval y in
            let _ = Printf.printf ", " in
            tup_iter req
      in
      let _ = Printf.printf "(" in
      let _ = tup_iter x in
      Printf.printf ")"
  | EntiteClass { name; _ } -> Printf.printf "<entite %s>" name
  | EntiteInst { name; attrs } ->
      let _ = Printf.printf "<inst entite %s" name in
      let _ =
        List.map
          (fun (n, v) ->
            let _ = Printf.printf " %s=" n in
            printval v)
          attrs
      in
      Printf.printf ">"
  | ComportementClass { entity1; name1; entity2; name2; condition; _ } -> (
      match condition with
      | Bool true ->
          Printf.printf "<Comportement %s[%s] <=> %s[%s]]>" entity1 name1
            entity2 name2
      | _ ->
          Printf.printf "<Comportement conditionel %s[%s] <=> %s[%s]]>" entity1
            name1 entity2 name2)

(* Environnement. *)
let error msg = raise (Failure msg)
let extend rho x v = (x, v) :: rho

let lookup var_name rho =
  try List.assoc var_name rho
  with Not_found -> error (Printf.sprintf "Undefined ident '%s'" var_name)

let unpacked_attrs attrs =
  List.map
    (fun x ->
      match x with
      | Attribut (attr, v) -> (attr, v))
    attrs

let merge_attrs default_attrs attrs =
  let rec parc x ret =
    match x with
    | [] -> ret
    | (attr, v) :: rem ->
        let w = (attr, try List.assoc attr attrs with Not_found -> v) in
        parc rem (w :: ret)
  in
  parc default_attrs []

let rec eval e rho =
  match e with
  | Int n -> Intval n
  | Bool b -> Boolval b
  | String s -> Stringval s
  | Ident v -> lookup v rho
  | App (e1, e2) -> (
      match (eval e1 rho, eval e2 rho) with
      | Funval { param; body; env }, v2 ->
          let rho1 = extend env param v2 in
          eval body rho1
      | (Funrecval { fname; param; body; env } as fval), v2 ->
          let rho1 = extend env fname fval in
          let rho2 = extend rho1 param v2 in
          eval body rho2
      | _, _ -> error "Apply a non-function")
  | Monop ("-", e) -> (
      match eval e rho with
      | Intval n -> Intval (-n)
      | _ -> error "Opposite of a non-integer")
  | Monop (op, _) -> error (Printf.sprintf "Unknown unary op: %s" op)
  | Binop (op, e1, e2) -> (
      match (op, eval e1 rho, eval e2 rho) with
      | "+", Intval n1, Intval n2 -> Intval (n1 + n2)
      | "-", Intval n1, Intval n2 -> Intval (n1 - n2)
      | "*", Intval n1, Intval n2 -> Intval (n1 * n2)
      | "/", Intval n1, Intval n2 -> Intval (n1 / n2)
      | ("+" | "-" | "*" | "/"), _, _ -> error "Arithmetic on non-integers"
      | "<", Intval n1, Intval n2 -> Boolval (n1 < n2)
      | ">", Intval n1, Intval n2 -> Boolval (n1 > n2)
      | "=", Intval n1, Intval n2 -> Boolval (n1 = n2)
      | "<=", Intval n1, Intval n2 -> Boolval (n1 <= n2)
      | ">=", Intval n1, Intval n2 -> Boolval (n1 >= n2)
      | ("<" | ">" | "=" | "<=" | ">="), _, _ ->
          error "Comparison of non-integers"
      | _ -> error (Printf.sprintf "Unknown binary op: %s" op))
  | If (e, e1, e2) -> (
      match eval e rho with
      | Boolval b -> eval (if b then e1 else e2) rho
      | _ -> error "Test on a non-boolean")
  | Fun (a, e) -> Funval { param = a; body = e; env = rho }
  | Let (x, e1, e2) ->
      let v1 = eval e1 rho in
      let rho1 = extend rho x v1 in
      eval e2 rho1
  | Letrec (f, x, e1, e2) ->
      let fval = Funrecval { fname = f; param = x; body = e1; env = rho } in
      let rho1 = extend rho f fval in
      eval e2 rho1
  | Tuple x -> Tupval (List.map (fun e -> eval e rho) x)
  | Entite (n, attrs) ->
      EntiteClass
        {
          name = n;
          attrs =
            List.map (fun (s, t) -> (s, eval t rho)) (unpacked_attrs attrs);
        }
  | EntiteVal (n, custom_attrs) ->
      let default_attrs =
        match lookup n rho with
        | EntiteClass { attrs; _ } -> attrs
        | _ -> raise (Failure "Type isn't an EntityClass")
      in
      let attrs =
        List.map (fun (s, t) -> (s, eval t rho)) (unpacked_attrs custom_attrs)
      in
      EntiteInst { name = n; attrs = merge_attrs default_attrs attrs }
  | EntiteAccess (n, attr) -> (
      let entity = lookup n rho in
      match entity with
      | EntiteInst { attrs; _ } -> (
          match List.assoc_opt attr attrs with
          | None -> raise (Failure "Unknown attribute")
          | Some x -> x)
      | _ ->
          raise
            (Failure
               "Cannot use access operator on an object that isn't a struct "))
  | Comportement (e1, n1, e2, n2, cond, instr) ->
      ComportementClass
        {
          entity1 = e1;
          name1 = n1;
          entity2 = e2;
          name2 = n2;
          condition = cond;
          instruction = instr;
        }

let eval e rho = eval e rho