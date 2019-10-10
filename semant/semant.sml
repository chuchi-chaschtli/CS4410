structure A = Absyn
structure T = Types
structure S = Symbol

signature ENV =
sig
  type access
  datatype enventry = VarEntry of {ty: T.ty}
                    | FunEntry of {formals: T.ty list, result : T.ty}
  val base_tenv : T.ty S.table (* predefined types *)
  val base_venv : enventry S.table (* predefined functions *)
end

structure Env :> ENV =
struct
  type access = unit
  datatype enventry = VarEntry of {ty: T.ty}
                    | FunEntry of {formals: T.ty list, result : T.ty}
  val base_tenv = S.empty (* predefined types *)
  val base_venv = S.empty (* predefined functions *)
end

structure Translate =
struct
  type exp = unit
end

(* signature SEMANTICS =
sig
  eqtype expty
  type venv
  type tenv

  (* val transVar : venv * tenv * A.var -> expty *)
  val transExp  : venv * tenv -> A.exp -> expty
  (* val transDec  : venv * tenv * A.dec -> {venv : venv, tenv : tenv} *)
  (* val transDecs : venv * tenv * A.dec list -> {venv : venv, tenv : tenv} *)
  (* val transTy  :        tenv * A.ty  -> T.ty *)
end *)

(* structure Semant :> SEMANTICS = *)
structure Semant =
struct
  type expty = {exp: Translate.exp, ty: T.ty}
  type venv = Env.enventry S.table
  type tenv = T.ty S.table

  (* val transVar : venv * tenv * A.var -> expty *)
  (* val transExp : venv * tenv -> A.exp -> expty *)
  (* val transDec  : venv * tenv * A.dec -> {venv : venv, tenv : tenv} *)
  (* val transDecs : venv * tenv * A.dec list -> {venv : venv, tenv : tenv} *)
  (* val transTy  :        tenv * A.ty  -> T.ty *)

  fun checkInt (ty, pos) =
    if ty = T.INT
    then ()
    else ErrorMsg.error pos ("expression must be an int, found: " ^ T.toString(ty) ^ " instead")

  fun checkUnit (ty, pos) =
    if ty = T.UNIT
    then ()
    else ErrorMsg.error pos ("expression must be a unit, found: " ^ T.toString(ty) ^ " instead")

  fun actual_ty typ =
    case typ of (T.NAME (_, ref(SOME inner))) => actual_ty inner
              | other                         => other;

  fun transExp(venv, tenv) =
    let
      fun trexp (A.OpExp{left, oper, right, pos}) =
          let
            val {exp=expLeft, ty=tyLeft} = trexp left
            val {exp=expRight, ty=tyRight} = trexp right
            fun verifyArithOperands() =
              (checkInt(tyLeft, pos);
               checkInt(tyRight, pos);
               {exp=((* TODO: do something with expLeft and expRight here? *)), ty=T.INT})
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
               (*| A.EqOp     =>  TODO: verify both sides can be compared *)
               (*| A.NeqOp    =>  TODO: verify both sides can be compared *)
          end
        | trexp (A.LetExp{decs, body, pos}) = {exp = (), ty = T.NIL}
            (* let val {venv = venv', tenv = tenv'} =
                transDecs(venv, tenv, decs)
            in
              transExp(venv', tenv') body
            end *)
        | trexp (A.VarExp(var)) = trvar(var)
        | trexp (A.NilExp) = {exp = (), ty = T.NIL}
        | trexp (A.IntExp(n)) = {exp = (), ty = T.INT}
        | trexp (A.StringExp(str, posn)) = {exp = (), ty = T.STRING}
        | trexp (A.CallExp{func, args, pos}) =
          (case S.look(venv, func)
              of SOME(Env.FunEntry{formals, result}) =>
                (let fun verifyFormals(firstFormal::restFormals, firstArg::restArgs) =
                          if (firstFormal = #ty (trexp firstArg))
                          then ()
                          else verifyFormals(restFormals, restArgs)
                      | verifyFormals(nil, nil) = ()
                      | verifyFormals(_, _) = ErrorMsg.error pos ("function formals length differs from arg length")
                in
                  verifyFormals(formals, args)
                end;
                {exp = (), ty = result})
              | NONE => (ErrorMsg.error pos ("undefined function " ^ S.name(func));
                          {exp = (), ty = T.UNIT}))
        (* | trexp (A.RecordExp{fields, typ, pos}) =
          let
          in
            (map trexp fields); (* TODO handle output of type checked fields (fold?) *)
            {exp = (), ty = T.RECORD}
          end (* Fold? *) *)
        | trexp (A.SeqExp(exprs)) =
          let fun verifyExprs nil = ()
                | verifyExprs ((expr, pos)::rest) = (trexp expr; verifyExprs(rest))
          in
            verifyExprs(exprs);
            {exp = (), ty = T.UNIT} (* TODO: Accumulate the translated expressions? *)
          end
        | trexp (A.AssignExp{var, exp, pos}) =
          let
            val {exp=exprExp, ty=exprTy} = trexp exp
            val {exp=varExp, ty=varTy} = trvar var
          in
            if exprTy = varTy
            then {exp = (), ty = varTy}
            else (ErrorMsg.error pos ("mismatching types within assignment ");  (* TODO report types *)
                 {exp = (), ty = T.INT})
          end
        | trexp (A.IfExp{test, then', else', pos}) =
          let
            val {exp=expTest, ty=tyTest} = trexp test
            val {exp=expBody, ty=tyThen} = trexp then'
          in
            checkInt(tyTest, pos);
            (case else'
              of SOME(expr) =>
                let
                  val {exp=expElse, ty=tyElse} = trexp expr
                in
                  if tyThen = tyElse
                  then {exp = (), ty = tyThen}
                  else (ErrorMsg.error pos ("then type " ^ T.toString(tyThen) ^ " does not match else type " ^ T.toString(tyElse));
                       {exp = (), ty = T.INT})
                end
              | NONE => {exp = (), ty = tyThen})
          end
        | trexp (A.WhileExp{test, body, pos}) =
          let
            val {exp=expTest, ty=tyTest} = trexp test
            val {exp=expBody, ty=tyBody} = trexp body
          in
            checkInt(tyTest, pos);
            checkUnit(tyBody, pos);
            {exp = (), ty = T.UNIT}
          end
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
          let
            val {exp=loExp, ty=tyLo} = trexp lo
            val {exp=hiExp, ty=tyHi} = trexp hi
            val venvUpdated = S.enter(venv, var, Env.VarEntry{ty=T.INT})
            val {exp=updatedExp, ty=updatedTy} = (transExp(venvUpdated, tenv) body)
          in
            checkInt(tyLo, pos);
            checkInt(tyHi, pos);
            checkUnit(updatedTy, pos);
		        {exp=(), ty=T.UNIT}
          end
        | trexp (A.BreakExp(pos)) = {exp = (), ty =  T.UNIT} (* TODO check our BREAK more thoroughly *)
        | trexp (A.ArrayExp{typ, size, init, pos}) =
          let
            val binding = S.look(tenv, typ)
            val {exp=expSize, ty=tySize} = trexp size
            val {exp=expInit, ty=tyInit} = trexp init
          in
            (case binding
              of SOME(ty) =>
                (case actual_ty ty
                  of (T.ARRAY(ty, unique)) =>
                     (checkInt(tySize, pos);
                     if actual_ty(ty) = actual_ty(tyInit)
                     then {exp = (), ty = T.ARRAY(ty, unique)}
                     else (ErrorMsg.error pos ("array type " ^ T.toString(tyInit)
                              ^ " does not match " ^ T.toString(ty));
                          {exp = (), ty = T.INT}))
                   | _ => (ErrorMsg.error pos ("type is not array " ^ S.name(typ));
                          {exp = (), ty = T.INT}))
              | NONE => (ErrorMsg.error pos ("undefined array type " ^ S.name(typ));
                         {exp = (), ty = T.INT}))
          end

      and trvar (A.SimpleVar(id, pos)) =
            (case S.look(venv, id)
                of SOME(Env.VarEntry{ty}) =>
                   {exp = (), ty = actual_ty ty}
                 | NONE => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
                            {exp = (), ty = T.INT}))
        | trvar (A.FieldVar(var, id, pos)) =
          let
            fun getFieldTypeWithId (nil, id, pos) =
                (ErrorMsg.error pos ("record does not have field with id: " ^ S.name id);
                T.UNIT)
              | getFieldTypeWithId ((name, ty)::rest, id, pos) =
                if (name = id)
                then ty
                else getFieldTypeWithId(rest, id, pos)
          in
            (case T.UNIT
              of T.RECORD (fields, unique) =>
                {exp=(), ty = getFieldTypeWithId(fields, id, pos)}
              | _ => (ErrorMsg.error pos ("Tried to access record field of object that is not a record");
                     {exp=(), ty = T.INT}))
          end
        | trvar (A.SubscriptVar(var, exp, pos)) =
          let
            val {exp=tyExp, ty=tyVar} = trvar(var)
          in
            case tyVar
              of T.ARRAY (ty, unique) => {exp=(), ty=ty}
               | _ => (ErrorMsg.error pos ("Attempted to access a non-array type: " ^ T.toString(tyVar));
                      {exp=(), ty=T.INT})
          end
    in
      trexp
    end

  (* fun transDec (venv, tenv, A.VarDec{name, typ=NONE, init, ...}) =
    let val {exp, ty} = transExp(venv, tenv) init
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


  fun transDecs (venv, tenv, decs) =
    let
      fun f({ve, te}, dec) = transDec(ve, te, dec)
    in
      foldl f {ve=venv, te=tenv} decs
    end *)
end
