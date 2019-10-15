structure A = Absyn
structure T = Types
structure S = Symbol

signature ENV =
sig
  type access
  datatype enventry = VarEntry of {ty: T.ty}
                    | ReadVarEntry of {ty: T.ty}
                    | FunEntry of {formals: T.ty list, result : T.ty}
  val base_tenv : T.ty S.table (* predefined types *)
  val base_venv : enventry S.table (* predefined functions *)
end

structure Env :> ENV =
struct
  type access = unit
  datatype enventry = VarEntry of {ty: T.ty}
                    | ReadVarEntry of {ty: T.ty}
                    | FunEntry of {formals: T.ty list, result : T.ty}

  fun populateEnvironment ((symbol, typ), table) = S.enter(table, S.symbol symbol, typ)

  val base_tenv = foldl populateEnvironment S.empty [("string", T.STRING), ("int", T.INT)]

  val base_venv = foldl populateEnvironment S.empty [("print", FunEntry ({formals=[T.STRING], result=T.UNIT})),
                                                     ("flush", FunEntry ({formals=[], result=T.UNIT})),
                                                     ("getchar", FunEntry ({formals=[], result=T.STRING})),
                                                     ("ord", FunEntry ({formals=[T.STRING], result=T.INT})),
                                                     ("chr", FunEntry ({formals=[T.INT], result=T.STRING})),
                                                     ("size", FunEntry ({formals=[T.STRING], result=T.INT})),
                                                     ("substring", FunEntry ({formals=[T.STRING, T.INT, T.INT], result=T.STRING})),
                                                     ("concat", FunEntry ({formals=[T.STRING, T.STRING], result=T.STRING})),
                                                     ("not", FunEntry ({formals=[T.INT], result=T.INT})),
                                                     ("exit", FunEntry ({formals=[T.INT], result=T.UNIT}))]
end

structure Translate =
struct
  type exp = unit
end

signature SEMANTICS =
sig
  type expty
  type venv
  type tenv
  val transTy   :        tenv * A.ty  -> T.ty
  val transDec  : venv * tenv * A.dec -> {venv : venv, tenv : tenv}
  val transDecs : venv * tenv * A.dec list -> {venv : venv, tenv : tenv}
  val transExp  : venv * tenv -> A.exp -> expty
  val transProg :                A.exp -> unit
end


structure Semant :> SEMANTICS =
struct
  type expty = {exp: Translate.exp, ty: T.ty}
  type venv = Env.enventry S.table
  type tenv = T.ty S.table

  (* bogus symbol to indicate whether or not we can break out of an expression.
     Tiger identifiers do not start with *, so we can safely ensure this will never
     exist in the actual AST. *)
  val breakable = S.symbol("*breakable")

  (* Checks if we can break here, and throws an error if we cannot. *)
  fun checkCanBreak(tenv, pos) =
    case S.look(tenv, breakable)
      of SOME _ => ()
       | NONE => ErrorMsg.error pos "break only allowed inside while/for expression"

  (* Checks if a given type is an INT, and throws an error if it is not *)
  fun checkInt (ty, pos) =
    if ty = T.INT
    then ()
    else ErrorMsg.error pos ("expression must be an int, found: " ^ T.toString(ty) ^ " instead")

  (* Checks if a given type is a UNIT, and throws an error if it is not *)
  fun checkUnit (ty, pos) =
    if ty = T.UNIT
    then ()
    else ErrorMsg.error pos ("expression must be a unit, found: " ^ T.toString(ty) ^ " instead")

  (* Checks if two types are equal, and throws an error if they are not *)
  fun checkEqual(ty1, ty2, pos) =
    if ty1 = ty2
      orelse (T.toString(ty1) = "RECORD" andalso ty2 = T.NIL)
      orelse (ty1 = T.NIL andalso T.toString(ty2) = "RECORD")
    then ()
    else ErrorMsg.error pos ("expression must be two comparable types, found: " ^ T.toString(ty1) ^ ", " ^ T.toString(ty2))

  (* Checks if a symbol list contains the given symbol *)
  (* symbol list * symbol -> bool *)
  fun contains(list, symbol) =
    let val name = S.name symbol
    in List.exists (fn elem => String.compare(S.name elem, name) = EQUAL) list
    end

  fun actual_ty typ =
    case typ of (T.NAME (_, ref(SOME inner))) => actual_ty inner
              | other                         => other;

  (* Used to check if a var can be re-assigned to *)
  (* A.var * Env.enventry S.table -> bool *)
  fun isVarAssignable (A.SimpleVar(id, pos), venv) =
        (case S.look(venv, id)
            of SOME(Env.ReadVarEntry{ty}) => false
             | _ => true)
    | isVarAssignable (A.FieldVar(var, id, pos), venv) =
      (case S.look(venv, id)
          of SOME(Env.ReadVarEntry{ty}) => false
           | _ => isVarAssignable(var, venv))
    | isVarAssignable (A.SubscriptVar(var, exp, pos), venv) = isVarAssignable(var, venv)

  fun transTy (tenv, A.NameTy(symbol, pos)) =
      (case S.look(tenv, symbol)
        of SOME ty => ty
         | NONE => (ErrorMsg.error pos ("type not found: " ^ S.name(symbol)); T.NIL))
    | transTy (tenv, A.ArrayTy(symbol, pos)) =
      (case S.look(tenv, symbol)
        of SOME ty => T.ARRAY(ty, ref ())
         | NONE => (ErrorMsg.error pos ("type not found: " ^ S.name(symbol)); T.NIL))
    | transTy (tenv, A.RecordTy(fieldList)) =
      T.RECORD(map
                (fn field => (#name field, transTy(tenv, A.NameTy(#typ field, #pos field))))
                fieldList,
              ref ())

  fun transExp(venv, tenv) =
    let
      fun trexp (A.OpExp{left, oper, right, pos}) =
          let
            val {exp=expLeft, ty=tyLeft} = trexp left
            val {exp=expRight, ty=tyRight} = trexp right
            fun verifyArithOperands() =
              (checkInt(tyLeft, pos);
               checkInt(tyRight, pos);
               {exp=(), ty=T.INT})
            fun verifyEquatableOperands() =
              (checkEqual(tyLeft, tyRight, pos);
               {exp=(), ty=T.INT})
            fun verifyComparableOperands() =
              (if (tyLeft = T.STRING andalso tyRight = T.STRING) orelse (tyLeft = T.INT andalso tyRight = T.INT)
               then ()
               else ErrorMsg.error pos "comparable types must be string or int";
                    {exp=(), ty=T.INT})
          in
            case oper
              of A.PlusOp   => verifyArithOperands()
               | A.MinusOp  => verifyArithOperands()
               | A.TimesOp  => verifyArithOperands()
               | A.DivideOp => verifyArithOperands()
               | A.LtOp     => verifyComparableOperands()
               | A.LeOp     => verifyComparableOperands()
               | A.GtOp     => verifyComparableOperands()
               | A.GeOp     => verifyComparableOperands()
               | A.EqOp     => verifyEquatableOperands()
               | A.NeqOp    => verifyEquatableOperands()
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
              of SOME(Env.FunEntry{formals, result}) =>
                (let fun verifyFormals(firstFormal::restFormals, firstArg::restArgs) =
                         let val firstArgExp = trexp firstArg
                         in
                            if (firstFormal = actual_ty(#ty (trexp firstArg)))
                            then verifyFormals(restFormals, restArgs)
                            else ErrorMsg.error pos ("type mismatch in function params: " ^ T.toString(firstFormal) ^ " and " ^ T.toString(#ty firstArgExp))
                         end
                       | verifyFormals(nil, nil) = ()
                       | verifyFormals(_, _) = ErrorMsg.error pos "function formals length differs from arg length"
                 in
                   verifyFormals(formals, args)
                 end;
                {exp = (), ty = result})
              | SOME _ => (ErrorMsg.error pos "environment entry is not a fun entry";
                           {exp = (), ty = T.UNIT})
              | NONE => (ErrorMsg.error pos ("undefined function " ^ S.name(func));
                         {exp = (), ty = T.UNIT}))
        | trexp (A.RecordExp{fields, typ, pos}) =
          (case S.look(tenv, typ)
            of SOME (T.RECORD(fieldList, unique)) =>
              let fun findId (id, nil) = (ErrorMsg.error pos "id not found"; NONE)
                    | findId (id, ((fieldId, ty)::rest)) = if (fieldId = id)
                                                           then SOME ty
                                                           else findId(id, rest)
                  fun searchFields((symbol, exp, pos)::rest) =
                      (case findId(symbol, fieldList)
                        of SOME ty =>
                            let val {exp = expField, ty = tyField} = trexp(exp)
                            in
                              if (tyField = ty)
                              then ()
                              else searchFields(rest)
                            end
                         | NONE => ErrorMsg.error pos "record field was undeclared")
                   | searchFields(nil) = ()
              in
                (searchFields fields;
                 {exp = (), ty = T.RECORD(fieldList, unique)})
              end
            | _ => (ErrorMsg.error pos "record type was undeclared";
                       {exp=(), ty=T.UNIT}))
        | trexp (A.SeqExp(exprs)) =
          let fun verifyExprs nil = ({exp = (), ty = T.UNIT})
                | verifyExprs ((expr, pos)::nil) = (trexp expr)
                | verifyExprs ((expr, pos)::rest) = (trexp expr; verifyExprs(rest))
          in
            verifyExprs(exprs)
          end
        | trexp (A.AssignExp{var, exp, pos}) =
          if isVarAssignable(var, venv)
          then
              let
                val {exp=exprExp, ty=exprTy} = trexp exp
                val {exp=varExp, ty=varTy} = trvar var
              in
                (checkEqual(exprTy, varTy, pos);
                 {exp = (), ty = T.UNIT})
              end
          else (ErrorMsg.error pos "cannot re-assign to var"; {exp = (), ty = T.UNIT})
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
                  (checkEqual(tyThen, tyElse, pos);
                   {exp = (), ty = tyThen})
                end
              | NONE => (checkUnit(tyThen, pos);
                         {exp = (), ty = T.UNIT}))
          end
        | trexp (A.WhileExp{test, body, pos}) =
          let
            val {exp=expTest, ty=tyTest} = trexp test
            val tenvUpdated = S.enter(tenv, breakable, T.UNIT)
            val {exp=expBody, ty=tyBody} = (transExp(venv, tenvUpdated) body)
          in
            checkInt(tyTest, pos);
            checkUnit(tyBody, pos);
            {exp = (), ty = T.UNIT}
          end
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
          let
            val {exp=loExp, ty=tyLo} = trexp lo
            val {exp=hiExp, ty=tyHi} = trexp hi
            val venvUpdated = S.enter(venv, var, Env.ReadVarEntry{ty=T.INT})
            val tenvUpdated = S.enter(tenv, breakable, T.UNIT)
            val {exp=updatedExp, ty=updatedTy} = (transExp(venvUpdated, tenvUpdated) body)
          in
            checkInt(tyLo, pos);
            checkInt(tyHi, pos);
            checkUnit(updatedTy, pos);
		        {exp=(), ty=T.UNIT}
          end
        | trexp (A.BreakExp(pos)) = (checkCanBreak(tenv, pos); {exp = (), ty =  T.UNIT})
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
                      checkEqual(actual_ty(ty), actual_ty(tyInit), pos);
                      {exp = (), ty = T.ARRAY(ty, unique)})
                   | _ => (ErrorMsg.error pos ("type is not array " ^ S.name(typ));
                          {exp = (), ty = T.INT}))
              | NONE => (ErrorMsg.error pos ("undefined array type " ^ S.name(typ));
                         {exp = (), ty = T.INT}))
          end

      and trvar (A.SimpleVar(id, pos)) =
            (case S.look(venv, id)
                of SOME(Env.VarEntry{ty}) =>
                   {exp = (), ty = actual_ty ty}
                 | SOME(Env.ReadVarEntry{ty}) =>
                    {exp = (), ty = actual_ty ty}
                 | SOME _ => (ErrorMsg.error pos "environment entry is not a var entry";
                              {exp = (), ty = T.INT})
                 | NONE => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
                            {exp = (), ty = T.INT}))
        | trvar (A.FieldVar(var, id, pos)) =
          let
            val {exp=expVar, ty=tyVar} = trvar(var)
            fun getFieldTypeWithId (nil, id, pos) =
                (ErrorMsg.error pos ("record does not have field with id: " ^ S.name id);
                T.UNIT)
              | getFieldTypeWithId ((name, ty)::rest, id, pos) =
                if (name = id)
                then ty
                else getFieldTypeWithId(rest, id, pos)
          in
            (case tyVar
              of T.RECORD (fields, unique) =>
                {exp=(), ty = getFieldTypeWithId(fields, id, pos)}
              | _ => (ErrorMsg.error pos "tried to access record field of object that is not a record";
                     {exp=(), ty = T.INT}))
          end
        | trvar (A.SubscriptVar(var, exp, pos)) =
          let
            val {exp=expVar, ty=tyVar} = trvar(var)
          in
            case tyVar
              of T.ARRAY (ty, unique) => {exp=(), ty=ty}
               | _ => (ErrorMsg.error pos ("Attempted to access a non-array type: " ^ T.toString(tyVar));
                      {exp=(), ty=T.INT})
          end
    in
      trexp
    end

  and transDec (venv, tenv, A.VarDec{name, typ, init, pos, ...}) =
      let val {exp, ty=tyInit} = transExp(venv, tenv) init
      in
        (case typ
          of SOME ty =>
             let val tyResult = transTy(tenv, A.NameTy(ty))
             in (checkEqual(tyInit, tyResult, pos);
                 {tenv = tenv, venv = S.enter(venv, name, Env.VarEntry{ty = tyResult})})
             end
           | NONE => (if (tyInit = T.NIL)
                      then (ErrorMsg.error pos "Cannot assign expression to nil")
                    	else ();
                      {tenv = tenv, venv = S.enter(venv, name, Env.VarEntry{ty = tyInit})}))
      end
    | transDec (venv, tenv, A.TypeDec(typeDecls)) = transTypeDecls(venv, tenv, typeDecls)
    | transDec (venv, tenv, A.FunctionDec(functionDecls)) = transFuncDecls(venv, tenv, functionDecls)

  and transTypeDecls (venv, tenv, typeDecls) =
    let
      val noneRefs = ref nil

      fun makeHeaderTenv ({name, ty, pos}, tenv) =
        (noneRefs := T.NAME(name, ref NONE) :: !noneRefs;
         S.enter(tenv, name, hd(!noneRefs)))
      val dummyTenv = foldl makeHeaderTenv tenv typeDecls

      fun transTyDec ({name, ty, pos}, {venv, tenv}) = {venv=venv, tenv=S.enter(tenv, name, transTy(tenv, ty))}
      val {venv=venv', tenv=tenv'} = foldl transTyDec {venv=venv, tenv=dummyTenv} typeDecls

      fun rewriteRef(T.NAME(symbol, tyRef)) =
        case S.look(tenv', symbol)
          of SOME(ty) => (tyRef := SOME(ty); nil)
           | NONE => (ErrorMsg.error 0 "referenced type not present in type environment"; nil) (* NOTE: should never occur *)

      fun verifyUnique({name, ty, pos}, visited) =
        if contains(visited, name)
        then (ErrorMsg.error pos "multiple matching type names in type declaration sequence"; visited)
        else name::visited

      fun verifyAcyclicSymbols({name, ty, pos}, visited) =
        (case S.look(tenv', name) of
          SOME (T.NAME(symbol, _)) => if contains(visited, symbol)
                                      then (ErrorMsg.error pos "cyclic mutually recursive types found"; visited)
                                      else symbol::visited
         | _ => visited)
    in
      foldl verifyUnique nil typeDecls;
      foldl verifyAcyclicSymbols nil typeDecls;
      map rewriteRef (!noneRefs);
      {venv=venv', tenv=tenv'}
    end

  and transFuncDecls (venv, tenv, functionDecls) =
    let
      fun transparam{name, escape, typ, pos} = case S.look(tenv, typ) of SOME t => {name=name, ty=t}

      fun verifyUnique({name, params, body, pos, result}, visited) =
        if contains(visited, name)
        then (ErrorMsg.error pos "multiple matching function names in function declaration sequence"; visited)
        else name::visited

      fun verifyReturnType({name, params, body, pos, result}, {venv, tenv}) =
        let val params' = map transparam params
            fun enterparam ({name, ty}, venv) = S.enter(venv, name, Env.VarEntry{ty=ty})
        in
          (case result
            of SOME(returnTy, returnPos) =>
               let val SOME(result_ty) = S.look(tenv, returnTy)
                   val venv' = S.enter(venv, name, Env.FunEntry{formals = map #ty params', result = result_ty})
                   val venv'' = foldl enterparam venv' params'
                   val {exp=funExp, ty=funTy} = transExp(venv'', tenv) body;
               in
                  checkEqual(funTy, result_ty, returnPos);
                  {venv=venv', tenv=tenv}
               end
             | NONE =>
               let val venv' = S.enter(venv, name, Env.FunEntry{formals = map #ty params', result = T.UNIT})
                   val venv'' = foldl enterparam venv' params'
                   val {exp=funExp, ty=funTy} = transExp(venv'', tenv) body;
               in
                  checkEqual(funTy, T.UNIT, pos);
                  {venv=venv', tenv=tenv}
               end)
        end

        fun dummyVenv ({name, params, body, pos, result}, venv) =
                S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params), result=T.UNIT})
        val venv' = foldl dummyVenv venv functionDecls
    in
      foldl verifyUnique nil functionDecls;
      foldl verifyReturnType {venv=venv', tenv=tenv} functionDecls
    end


  and transDecs (venv, tenv, decs) =
    let
      fun f({ve, te}, nil) = (ErrorMsg.error 0 "empty declaration list"; {venv=venv, tenv=tenv}) (* NOTE should never occur *)
        | f({ve, te}, dec::nil) = transDec(ve, te, dec)
        | f({ve, te}, dec::decs) =
          let val {venv=venv', tenv=tenv'} = transDec(ve, te, dec)
          in
            f({ve=venv', te=tenv'}, decs)
          end
    in
      f ({ve=venv, te=tenv}, decs)
    end

  fun transProg exp = (transExp(Env.base_venv, Env.base_tenv) exp; ())
end
