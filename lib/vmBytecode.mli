type vm_code = vm_instr list

and vm_instr =
  | VMI_Loadi of int
  | VMI_Loadb of bool
  | VMI_Loads of string
  | VMI_Plus
  | VMI_Sub
  | VMI_Mult
  | VMI_Div
  | VMI_Equal
  | VMI_Access of int
  | VMI_Branch of (vm_code * vm_code)
  | VMI_Push
  | VMI_Swap
  | VMI_Mkclos of vm_code
  | VMI_Apply

type vm_val =
  | VMV_int of int
  | VMV_bool of bool
  | VMV_string of string
  | VMV_closure of
      (vm_code
      * vm_val (* Assumed to be an environment value, i.e. [VMV_env] *))
  | VMV_env of vm_val list
  | VMV_code_addr of vm_code

type vw_stack = vm_val list
type vm_state = { register : vm_val; code : vm_code; stack : vw_stack }
