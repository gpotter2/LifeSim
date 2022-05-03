exception Computation_success of VmBytecode.vm_val
exception Computation_failure

(* Print a state of the virtual machine. *)
let pp_state ppf state =
  Printf.fprintf ppf "Register r: %a\n%!" PrintByteCode.pp_value
    state.VmBytecode.register;
  Printf.fprintf ppf "Code:\n%aEnd of code%!\n" PrintByteCode.pp_code
    state.VmBytecode.code;
  Printf.fprintf ppf "Stack:\n";
  List.iter
    (fun v -> Printf.fprintf ppf "%a\n%!" PrintByteCode.pp_value v)
    state.VmBytecode.stack;
  Printf.fprintf ppf "End of stack\n%!"

(* Compute the next state of the machine from the current state. *)
let next_state state =
  match
    (state.VmBytecode.register, state.VmBytecode.code, state.VmBytecode.stack)
  with
  | _, VmBytecode.VMI_Loadi i :: c, s' ->
      (* Int constant. *)
      {
        VmBytecode.register = VmBytecode.VMV_int i;
        VmBytecode.code = c;
        VmBytecode.stack = s';
      }
  | _, VmBytecode.VMI_Loadb b :: c, s' ->
      (* Bool constant. *)
      {
        VmBytecode.register = VmBytecode.VMV_bool b;
        VmBytecode.code = c;
        VmBytecode.stack = s';
      }
  | _, VmBytecode.VMI_Loads str :: c, s' ->
      (* String constant. *)
      {
        VmBytecode.register = VmBytecode.VMV_string str;
        VmBytecode.code = c;
        VmBytecode.stack = s';
      }
  | VmBytecode.VMV_int i, VmBytecode.VMI_Plus :: c, VmBytecode.VMV_int j :: s'
    ->
      (* Addition. *)
      {
        VmBytecode.register = VmBytecode.VMV_int (i + j);
        VmBytecode.code = c;
        VmBytecode.stack = s';
      }
  | VmBytecode.VMV_int i, VmBytecode.VMI_Sub :: c, VmBytecode.VMV_int j :: s' ->
      (* Subtraction. *)
      {
        VmBytecode.register = VmBytecode.VMV_int (i - j);
        VmBytecode.code = c;
        VmBytecode.stack = s';
      }
  | VmBytecode.VMV_int i, VmBytecode.VMI_Mult :: c, VmBytecode.VMV_int j :: s'
    ->
      (* Multiplication. *)
      {
        VmBytecode.register = VmBytecode.VMV_int (i * j);
        VmBytecode.code = c;
        VmBytecode.stack = s';
      }
  | VmBytecode.VMV_int i, VmBytecode.VMI_Div :: c, VmBytecode.VMV_int j :: s' ->
      (* Division. *)
      {
        VmBytecode.register = VmBytecode.VMV_int (i / j);
        VmBytecode.code = c;
        VmBytecode.stack = s';
      }
  | VmBytecode.VMV_int i, VmBytecode.VMI_Equal :: c, VmBytecode.VMV_int j :: s'
    ->
      (* Comparison assumed to be between 2 integers. *)
      {
        VmBytecode.register = VmBytecode.VMV_bool (i = j);
        VmBytecode.code = c;
        VmBytecode.stack = s';
      }
  | ( VmBytecode.VMV_bool i,
      VmBytecode.VMI_Equal :: c,
      VmBytecode.VMV_bool j :: s' ) ->
      (* Comparison assumed to be between 2 booleans. *)
      {
        VmBytecode.register = VmBytecode.VMV_bool (i = j);
        VmBytecode.code = c;
        VmBytecode.stack = s';
      }
  | ( VmBytecode.VMV_string str1,
      VmBytecode.VMI_Equal :: c,
      VmBytecode.VMV_string str2 :: s' ) ->
      (* Comparison assumed to be between 2 strings. *)
      {
        VmBytecode.register = VmBytecode.VMV_bool (str1 = str2);
        VmBytecode.code = c;
        VmBytecode.stack = s';
      }
  | VmBytecode.VMV_bool true, VmBytecode.VMI_Branch (c1, _) :: c, r :: s ->
      (* Branch if true. *)
      {
        VmBytecode.register = r;
        VmBytecode.code = c1;
        VmBytecode.stack = VmBytecode.VMV_code_addr c :: s;
      }
  | VmBytecode.VMV_bool false, VmBytecode.VMI_Branch (_, c2) :: c, r :: s ->
      (* Branch if false. *)
      {
        VmBytecode.register = r;
        VmBytecode.code = c2;
        VmBytecode.stack = VmBytecode.VMV_code_addr c :: s;
      }
  | r, VmBytecode.VMI_Push :: c, s ->
      (* Push onto the stack. *)
      {
        VmBytecode.register = r;
        VmBytecode.code = c;
        VmBytecode.stack = r :: s;
      }
  | r1, VmBytecode.VMI_Swap :: c, r2 :: s ->
      (* Swap top stack and register. *)
      {
        VmBytecode.register = r2;
        VmBytecode.code = c;
        VmBytecode.stack = r1 :: s;
      }
  | r, VmBytecode.VMI_Mkclos body_code :: c, s ->
      (* Make a closure. *)
      {
        VmBytecode.register = VmBytecode.VMV_closure (body_code, r);
        VmBytecode.code = c;
        VmBytecode.stack = s;
      }
  | ( r,
      VmBytecode.VMI_Apply :: c,
      VmBytecode.VMV_closure (body_code, VmBytecode.VMV_env r0) :: s ) ->
      (* Application. *)
      {
        VmBytecode.register = VMV_env (r :: r0);
        VmBytecode.code = body_code;
        VmBytecode.stack = VMV_code_addr c :: s;
      }
  | VmBytecode.VMV_env lst, VmBytecode.VMI_Access n :: c, s ->
      (* Identifier. *)
      {
        VmBytecode.register = List.nth lst n;
        VmBytecode.code = c;
        VmBytecode.stack = s;
      }
  | r, [ (* Return *) ], VmBytecode.VMV_code_addr c :: s ->
      (* Return from function call, continue pending code. *)
      { VmBytecode.register = r; VmBytecode.code = c; VmBytecode.stack = s }
  | r, [ (* Return *) ], [] ->
      (* No more code to execute : the end. *)
      raise (Computation_success r)
  | _, _, _ ->
      Printf.eprintf "State trace:\n%a%!" pp_state state;
      raise Computation_failure
