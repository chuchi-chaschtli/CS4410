structure A = Absyn
structure T = Types
structure S = Symbol

signature ENV =
sig
    type access
    type ty
    datatype enventry = VarEntry of {ty: ty}
                      | FunEntry of {formals: ty list, result : ty}
    val base_tenv : ty S.table (* predefined types *)
    val base_venv : enventry S.table (* predefined functions *)
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

structure Translate =
struct
  struct type exp = unit
end

structure Semant =
struct
  type venv = Env.enventry S.empty
  type tenv = ty S.empty

  type expty = {exp: Translate.exp, ty: T.ty}

  transVar : venv * tenv * A.var -> expty
  transExp : venv * tenv * A.exp -> expty
  transDec : venv * tenv * A.dec -> {venv : venv, tenv : tenv}
  transTy :         tenv * A.ty  -> T.ty
end
