exception Compilation_not_implemented
exception Unbound_identifier of string

(* Generate bytecode for a general expression. *)
let rec compile_expr rho = function
  | Pcfast.Bool b -> [ VmBytecode.VMI_Loadb b ]
  | Pcfast.Int i -> [ VmBytecode.VMI_Loadi i ]
  | Pcfast.String s -> [ VmBytecode.VMI_Loads s ]
  | Pcfast.If (e1, e2, e3) ->
      (VmBytecode.VMI_Push :: compile_expr rho e1)
      @ [ VmBytecode.VMI_Branch (compile_expr rho e2, compile_expr rho e3) ]
  | Pcfast.Ident id ->
      let rec next x i =
        match x with
        | q :: rem -> if q = id then i else next rem (i + 1)
        | [] -> raise (Failure "Could not find varname")
      in
      [ VmBytecode.VMI_Access (next rho 0) ]
  | Pcfast.Monop (o_name, e) -> (
      match o_name with
      | "-" -> compile_expr rho (Pcfast.Binop ("-", Pcfast.Int 0, e))
      | _ -> raise (Failure "compile_expr: monop not implemented"))
  | Pcfast.Binop (o_name, e1, e2) ->
      (VmBytecode.VMI_Push :: compile_expr rho e2)
      @ (VmBytecode.VMI_Swap :: compile_expr rho e1)
      @ [
          (match o_name with
          | "+" -> VmBytecode.VMI_Plus
          | "-" -> VmBytecode.VMI_Sub
          | "*" -> VmBytecode.VMI_Mult
          | "/" -> VmBytecode.VMI_Div
          | "=" -> VmBytecode.VMI_Equal
          | _ -> raise (Failure "compile_expr: binop not implemented"));
        ]
  | Pcfast.Fun (x, body) ->
      [ VmBytecode.VMI_Mkclos (compile_expr (x :: rho) body) ]
  | Pcfast.App (e1, e2) ->
      (VmBytecode.VMI_Push :: compile_expr rho e1)
      @ (VmBytecode.VMI_Swap :: compile_expr rho e2)
      @ [ VmBytecode.VMI_Apply ]
  | Pcfast.Let (x, e1, e2) ->
      compile_expr rho (Pcfast.App (Pcfast.Fun (x, e2), e1))
  | _ -> raise Compilation_not_implemented
