exception Compilation_not_implemented
exception Unbound_identifier of string

val compile_expr : string list -> Pcfast.expr -> VmBytecode.vm_code
