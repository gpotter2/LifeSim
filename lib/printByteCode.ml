let rec __pp_code ppf depth code = List.iter (__pp_instr ppf depth) code

and __pp_instr ppf depth instr =
  (* Print indentation. *)
  let curr_indent = String.make (2 * depth) ' ' in
  Printf.fprintf ppf "%s" curr_indent;
  match instr with
  | VmBytecode.VMI_Loadi i -> Printf.fprintf ppf "Loadi %d\n%!" i
  | VmBytecode.VMI_Loadb b -> Printf.fprintf ppf "Loadb %b\n%!" b
  | VmBytecode.VMI_Loads s -> Printf.fprintf ppf "Loads %s\n%!" s
  | VmBytecode.VMI_Plus -> Printf.fprintf ppf "Plus\n%!"
  | VmBytecode.VMI_Sub -> Printf.fprintf ppf "Sub\n%!"
  | VmBytecode.VMI_Mult -> Printf.fprintf ppf "Mult\n%!"
  | VmBytecode.VMI_Div -> Printf.fprintf ppf "Div\n%!"
  | VmBytecode.VMI_Equal -> Printf.fprintf ppf "Equal\n%!"
  | VmBytecode.VMI_Access i -> Printf.fprintf ppf "Access %d\n%!" i
  | VmBytecode.VMI_Branch (c1, c2) ->
      Printf.fprintf ppf "Branch then\n%!";
      __pp_code ppf (depth + 1) c1;
      Printf.fprintf ppf "%sBranch else\n%!" curr_indent;
      __pp_code ppf (depth + 1) c2;
      Printf.fprintf ppf "%sBranch end\n%!" curr_indent
  | VmBytecode.VMI_Push -> Printf.fprintf ppf "Push\n%!"
  | VmBytecode.VMI_Swap -> Printf.fprintf ppf "Swap\n%!"
  | VmBytecode.VMI_Mkclos c ->
      Printf.fprintf ppf "Mkclos\n%!";
      __pp_code ppf (depth + 1) c;
      Printf.fprintf ppf "%sMkclos end\n%!" curr_indent
  | VmBytecode.VMI_Apply -> Printf.fprintf ppf "Apply\n%!"

let rec pp_separated_list ppf printer = function
  | [] -> ()
  | [ last ] -> Printf.fprintf ppf "%a" printer last
  | h :: q ->
      Printf.fprintf ppf "%a, " printer h;
      pp_separated_list ppf printer q

(* Print a virtual machine value. *)
let rec pp_value ppf = function
  | VmBytecode.VMV_int i -> Printf.fprintf ppf "%d" i
  | VmBytecode.VMV_bool b -> Printf.fprintf ppf "%b" b
  | VmBytecode.VMV_string s -> Printf.fprintf ppf "%s" s
  | VmBytecode.VMV_closure (_body, _env) ->
      Printf.fprintf ppf "<fun>"
      (* Otherwise, for full print, cumbersome...
         Printf.fprintf ppf "<fun (" ;
         pp_separated_list ppf (fun _ppf -> (__pp_instr _ppf 0)) body ;
         Printf.fprintf ppf ", %a)>" pp_value env
      *)
  | VmBytecode.VMV_env vals ->
      Printf.fprintf ppf "<env> = [";
      pp_separated_list ppf pp_value vals;
      Printf.fprintf ppf "]"
  | VmBytecode.VMV_code_addr _ -> Printf.fprintf ppf "<code>"

(* Print a virtual machine bytecode program. Only exported function
   to hide the extra indentation parameter that must always be 0 at
   the initiall call. *)
let pp_code ppf code = __pp_code ppf 0 code
