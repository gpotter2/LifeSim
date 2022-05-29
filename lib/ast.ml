(* Ce fichier contient la définition du type OCaml des arbres de
 * syntaxe abstraite du langage, ainsi qu'un imprimeur des phrases
 * du langage.
 *)

type expr =
  | Int of int
  | Bool of bool
  | String of string
  | Ident of string
  | App of expr * expr
  | Monop of string * expr
  | Binop of string * expr * expr
  | Tuple of expr list
  | If of expr * expr * expr
  | Fun of string * expr
  | Let of string * expr * expr
  | Letrec of string * string * expr * expr
  | Comportement of string * string * string * string * expr * expr
  | Entite of (string * attribut list)
  | EntiteVal of (string * attribut list)

and attribut = Attribut of string * expr

open Printf

let rec un_body params = function
  | Fun (p, e) -> un_body (p :: params) e
  | e -> (params, e)

let rec print oc e =
  match e with
  | Int n -> fprintf oc "%d" n
  | Bool b -> fprintf oc "%s" (if b then "T" else "F")
  | Ident s -> fprintf oc "%s" s
  | String s -> fprintf oc "\"%s\"" s
  | App (e1, e2) -> fprintf oc "(%a %a)" print e1 print e2
  | Let (f, e1, e2) ->
      let params, e = un_body [] e1 in
      fprintf oc "(let %s %a= %a in %a)" f
        (fun oc -> List.iter (fun s -> fprintf oc "%s " s))
        params print e print e2
  | Letrec (f, x, e1, e2) ->
      let params, e = un_body [] e1 in
      Printf.fprintf oc "(let rec %s %s %a= %a in %a)" f x
        (fun oc -> List.iter (fun s -> Printf.fprintf oc "%s " s))
        params print e print e2
  | Fun (x, e) -> fprintf oc "(fun %s -> %a)" x print e
  | If (test, e1, e2) ->
      fprintf oc "(if %a then %a else %a)" print test print e1 print e2
  | Binop (op, e1, e2) -> fprintf oc "(%a %s %a)" print e1 op print e2
  | Monop (op, e) -> fprintf oc "%s%a" op print e
  | Entite (name, _) -> fprintf oc "(entite %s)" name
  | EntiteVal (name, _) -> fprintf oc "(entite_val %s)" name
  | Tuple tup_expr ->
      let rec tup_iter oc x =
        match x with
        | [] -> ()
        | [ y ] -> fprintf oc "%a" print y
        | y :: req ->
            let _ = fprintf oc "%a, " print y in
            tup_iter oc req
      in
      fprintf oc "(%a)" (fun oc -> tup_iter oc) tup_expr
  | Comportement (e1, n1, e2, n2, cond, _) -> (
      match cond with
      | Bool true -> fprintf oc "(comportement %s[%s] <=> %s[%s])" e1 n1 e2 n2
      | _ ->
          fprintf oc "(comportement conditionel %s[%s] <=> %s[%s])" e1 n1 e2 n2)
