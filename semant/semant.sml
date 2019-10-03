structure A = Absyn
structure T = Types
structure S = Symbol

signature ENV =
sig
    type access
    type ty
    datatype enventry = VarEntry of {ty: ty}
                      | FunEntry of {formals: ty list, result : ty}
    val base_tenv : ty Symbol.table (* predefined types *)
    val base_venv : enventry Symbol.table (* predefined functions *)
end

structure Env :> ENV =
struct
    type access = unit
    type ty = T.ty

    datatype enventry = VarEntry of {ty: ty}
                      | FunEntry of {formals: ty list, result : ty}

    val base_tenv = S.empty (* predefined types *)
    val base_venv = S.empty (* predefined functions *)
end
