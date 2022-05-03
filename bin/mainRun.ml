open Vol

(* Boolean telling wether the execution must be traced, i.e. wether the state
    of the machine must be printed at each execution step. This boolean is
    set by the "-t" option passed on the command line. *)
let trace_flag = ref false
let step_count = ref 0
let initial_env = VmBytecode.VMV_env []

let load_run_and_quit fname =
  let in_handle = open_in_bin fname in
  (* Get the bytecode of the program. *)
  let (prog : VmBytecode.vm_code) =
    try input_value in_handle
    with Failure _ ->
      Printf.printf "Error: bad bytecode format.\n%!";
      exit (-2)
  in
  (* Initial state. *)
  let state =
    ref
      {
        VmBytecode.register = initial_env;
        VmBytecode.code = prog;
        VmBytecode.stack = [];
      }
  in
  if !trace_flag then
    Printf.printf "Initial WM state is:\n%a\n%!" VmExec.pp_state !state;
  try
    while true do
      incr step_count;
      state := VmExec.next_state !state;
      if !trace_flag then
        Printf.printf "After step %d WM state is:\n%a\n%!" !step_count
          VmExec.pp_state !state
    done
  with
  | VmExec.Computation_success v ->
      Printf.printf "Computation success: %a\n%!" PrintByteCode.pp_value v;
      exit 0
  | VmExec.Computation_failure ->
      Printf.printf "Computation failure\n%!";
      exit (-1)
;;

Arg.parse
  [
    ( "-t",
      Arg.Unit (fun () -> trace_flag := true),
      "Enable virtual machine trace during execution" );
  ]
  (fun f -> load_run_and_quit f)
  "Runs the bytecode file provided in argument through the PCF virtual machine."
