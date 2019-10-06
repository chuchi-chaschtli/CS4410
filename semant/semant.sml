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

  fun checkInt ({exp, ty}, pos) =
    if ty = Types.INT
    then ()
    else ErrorMsg.error pos "expression " exp ^ " must be an int"

  fun transExp(venv, tenv) =
    let
      fun trexp (A.OpExp{left, oper, right, pos}) =
          let
            val {exp=expLeft, ty=tyLeft} = trexp left
            val {exp=expRight, ty=tyRight} = trexp right
            fun verifyArithOperands =
              (checkInt(tyLeft, pos);
               checkInt(tyRight, pos);
               {exp=(), ty=Types.INT})
          in
            case oper
              of A.PlusOp   => verifyArithOperands()
               | A.MinusOp  => verifyArithOperands()
               | A.TimesOp  => verifyArithOperands()
               | A.DivideOp => verifyArithOperands()
               | A.LtOp     => verifyArithOperands()
               | A.LeOp     => verifyArithOperands()
               | A.GtOp     => verifyArithOperands()
               | A.GeOp     => verifyArithOperands()
               | A.EqOp     => (* TODO: verify both sides can be compared *)
               | A.NeqOp    => (* TODO: verify both sides can be compared *)
          end
        | trexp (A.LetExp{decs, body, pos}) =
            let val {venv = venv', tenv = tenv'} =
                transDecs(venv, tenv, decs)
            in
              transExp(venv', tenv') body
            end
        | trexp (A.VarExp(var)) = trvar(var)
        | trexp (A.NilExp) = {exp = (), ty = T.NIL}
        | trexp (A.IntExp(n)) = {exp = (), ty = T.INT}
        | trexp (A.StringExp(str, posn)) = {exp = (), ty = T.STRING}
        | trexp (A.CallExp{func, args, pos}) =
          (case S.look(venv, func)
              of SOME(Env.FunEntry{formals = ty list, result = ty}) =>
                  (map trexp formals; (* TODO handle output of type checked args *)
                   fold {exp = (), ty = result}) (* Fold? *)
              | NONE => (ErrorMsg.error pos ("undefined function " ^ S.name func);
                          {exp = (), ty = T.UNIT}))
        | trexp (A.RecordExp{fields, typ, pos}) =
          (map trexp field; (* TODO handle output of type checked fields (fold?) *)
            {exp = (), ty = T.RECORD}) (* Fold? *)
        | trexp (A.SeqExp{exprs}) =
          ((map trexp exprs; (* TODO handle output of type checked exprs (fold?) *)
            {exp = (), ty = T.UNIT}) (* Fold? *))
        | trexp (A.AssignExp{var, expr, pos}) =
          let
            val e = trexp expr
            val v = trvar var
          in
            if e.ty = v.ty
            then {exp = (), ty = v.ty}
            else (ErrorMsg.error pos "mismatching types within assignment ");  (* TODO report types *)
                 {exp = (), ty = T.INT}
          end
        | trexp (A.IfExp{test, then', else', pos}) =
          let
            val condType = trexp test
            val thenType = trexp then'
          in
            if condType.ty = T.INT
            then
              (case else'
                of SOME(expr) =>
                  if trexp(expr).ty = thenType.ty
                  then {exp = (), ty = thenType.ty}
                  else (ErrorMsg.error pos "mismatching then-else types within if ");  (* TODO report types *)
                       {exp = (), ty = T.INT}
                | NONE => {exp = (), ty = thenType.ty})
            else (ErrorMsg.error pos "non-int conditional within if ");  (* TODO report cond type *)
                 {exp = (), ty = T.INT}
          end
        | trexp (A.WhileExp{test, body, pos}) =
          let
            val condType = trexp test
            val bodyType = trexp body
          in
            if condType.ty = T.INT
            then
              if bodyType.ty = T.UNIT
              then {exp = (), ty = T.UNIT}
              else (ErrorMsg.error pos "non-void body within while ");  (* TODO report body type *)
                   {exp = (), ty = T.INT}
            else (ErrorMsg.error pos "non-int conditional within while ");  (* TODO report cond type *)
                 {exp = (), ty = T.INT}
          end
        | trexp (A.ForExp ...) ...
        | trexp (A.BreakExp) = {exp = (), ty =  T.UNIT} (* TODO check our BREAK more thoroughly *)
        | trexp (A.ArrayExp{typ, size, init, pos}) =
          let
            val typType = S.look tenv typ
            val sizeType = trexp size
            val initType = trexp init
          in
            (case typType
              of SOME(Env.VarEntry{ty}) =>
                if ty = T.ARRAY{innerTy, unique}
                then
                  if sizeType.ty = T.INT
                  then
                    if initType.ty = innerTy
                    then {exp = (), ty = T.ARRAY{ty, unique}}
                    else (ErrorMsg.error pos ("array type does not match "); (* TODO Should be type of inner *)
                          {exp = (), ty = T.INT}))
                  else (ErrorMsg.error pos ("array size is not int "); (* TODO report size type *)
                        {exp = (), ty = T.INT}))
                else (ErrorMsg.error pos ("type is not array " ^ S.name typ);
                      {exp = (), ty = T.INT}))
              | NONE => (ErrorMsg.error pos ("undefined array type " ^ S.name typ);
                         {exp = (), ty = T.INT}))
          end

      and trvar (A.SimpleVar(id, pos)) =
            (case S.look(venv, id)
                of SOME(Env.VarEntry{ty}) =>
                   {exp = (), ty = actual_ty ty}
                 | NONE => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
                            {exp = (), ty = T.INT}))
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


    fun transDecs(venv, tenv, l) =
      let
        fun f({ve, te}, dec) = transDec(ve, te, dec)
      in
        foldl f l {venv, tenv}
      end
end
