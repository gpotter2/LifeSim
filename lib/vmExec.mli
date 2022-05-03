exception Computation_success of VmBytecode.vm_val
exception Computation_failure

val pp_state : out_channel -> VmBytecode.vm_state -> unit
val next_state : VmBytecode.vm_state -> VmBytecode.vm_state
