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

  fun checkInt ({exp, ty}, pos) = (...)

  fun transExp(venv, tenv) =
    let
      fun trexp (A.OpExp{left, oper = A.PlusOp, right, pos}) =
                (checkInt(trexp left, pos);
                 checkInt(trexp right, pos);
                 {exp = (), ty = T.INT})
        | trexp (A.LetExp{decs, body, pos}) =
            let val {venv = venv', tenv = tenv'} =
                transDec(venv, tenv, decs)
              in transExp(venv', tenv') body
            end
        | trexp (A.VarExp ...) ...
        | trexp (A.NilExp ...) ...
        | trexp (A.IntExp ...) ...
        | trexp (A.StringExp ...) ...
        | trexp (A.CallExp ...) ...
        | trexp (A.RecordExp ...) ...
        | trexp (A.SeqExp ...) ...
        | trexp (A.AssignExp ...) ...
        | trexp (A.IfExp ...) ...
        | trexp (A.WhileExp ...) ...
        | trexp (A.ForExp ...) ...
        | trexp (A.BreakExp ...) ...
        | trexp (A.LetExp ...) ...
        | trexp (A.ArrayExp ...) ...

      and trvar (A.SimpleVar(id, pos)) =
            (case S.look(venv, id)
                of SOME(E.VarEntry{ty}) =>
                   {exp = (), ty = actual_ty ty}
                | NONE => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
                           exp = (), ty = T.INT))
        | trvar (A.FieldVar(v, id, pos)) = ...
        | trvar (A.SubscriptVar(v, exp, pos)) = ...
    in
      trexp
    end

  fun transDec (venv, tenv, A.VarDec{name, typ=NONE, init, ...}) =
    let val {exp, ty} = transExp(venv, tenv, init)
      in {tenv = tenv, venv = S.enter(venv, name, Env.VarEntry{ty = ty})}
    end
    | transDec (venv, tenv, A.TypeDec[{name, ty}]) =
      {venv = venv, tenv = S.enter(tenv, name, transTy(tenv, ty))}
    | transDec (venv, tenv, A.FunctionDec[{name, params, body, pos, result = SOME(rt, pos)}]) =
      let val SOME(result_ty) = S.look(tenv, rt)
          fun transparam{name, typ, pos} = case S.look(tenv, typ) of SOME t => {name=name, ty=t}
          val params' = map transparam params
          val venv' = S.enter(venv, name, Env.FunEntry{formals = map #ty params', result = result_ty})
          fun enterparam ({name, ty}, venv) = S.enter(venv, name, Env.VarEntry{access=(), ty=ty})
          val venv'' = fold enterparam params' venv'
        in transExp(venv'', tenv) body;
           {venv = venv', tenv = tenv}
      end
end
