structure A = Absyn
structure T = Types
structure S = Symbol
structure IR = Translate

signature ENV =
sig
  type access

  datatype enventry = VarEntry     of {access: IR.access, ty: T.ty}
                    | ReadVarEntry of {access: IR.access, ty: T.ty}
                    | FunEntry     of {level: IR.level,
                                       label: Temp.label,
                                       formals: T.ty list, result : T.ty}

  val base_tenv : T.ty S.table (* predefined types *)
  val base_venv : enventry S.table (* predefined functions *)

end

structure Env :> ENV =
struct
  type access = unit

  datatype enventry = VarEntry     of {access: IR.access, ty: T.ty}
                    | ReadVarEntry of {access: IR.access, ty: T.ty}
                    | FunEntry     of {level: IR.level,
                                       label: Temp.label,
                                       formals: T.ty list, result: T.ty}

  fun populateEnvironment ((symbol, typ), table) = S.enter(table, S.symbol symbol, typ)

  val base_tenv = foldl populateEnvironment S.empty [("string", T.STRING), ("int", T.INT)]

  fun globalFunEntry (formals, result) = FunEntry{level = IR.outermost,
                                                  label = Temp.newlabel(),
                                                  formals = formals, result = result}

  val base_venv = foldl populateEnvironment S.empty [("print", globalFunEntry([T.STRING], T.UNIT)),
                                                     ("flush", globalFunEntry([], T.UNIT)),
                                                     ("getchar", globalFunEntry([], T.STRING)),
                                                     ("ord", globalFunEntry([T.STRING], T.INT)),
                                                     ("chr", globalFunEntry([T.INT], T.STRING)),
                                                     ("size", globalFunEntry([T.STRING], T.INT)),
                                                     ("substring", globalFunEntry([T.STRING, T.INT, T.INT], T.STRING)),
                                                     ("concat", globalFunEntry([T.STRING, T.STRING], T.STRING)),
                                                     ("not", globalFunEntry([T.INT], T.INT)),
                                                     ("exit", globalFunEntry([T.INT], T.UNIT))]
end

signature SEMANTICS =
sig
  type expty
  type venv
  type tenv
  val transTy   :        tenv            * A.ty       -> T.ty
  val transDec  : venv * tenv * IR.level * A.dec      -> {venv : venv, tenv : tenv}
  val transDecs : venv * tenv * IR.level * A.dec list -> {venv : venv, tenv : tenv}
  val transExp  : venv * tenv * IR.level              -> A.exp -> expty
  val transProg : A.exp -> unit
end


structure Semant :> SEMANTICS =
struct
  type expty = {exp: IR.exp, ty: T.ty}
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

  (* Checks if two types are equal *)
  fun checkEqual(ty1, ty2, pos) =
    ty1 = ty2
      orelse (T.toString(ty1) = "RECORD" andalso ty2 = T.NIL)
      orelse (ty1 = T.NIL andalso T.toString(ty2) = "RECORD")

  (* Checks if two types are equal, and throws an error if they are not *)
  fun checkEqualOrThrow(ty1, ty2, pos) =
    if checkEqual(ty1, ty2, pos)
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
            of SOME(Env.ReadVarEntry{access, ty}) => false
             | _ => true)
    | isVarAssignable (A.FieldVar(var, id, pos), venv) =
      (case S.look(venv, id)
          of SOME(Env.ReadVarEntry{access, ty}) => false
           | _ => isVarAssignable(var, venv))
    | isVarAssignable (A.SubscriptVar(var, exp, pos), venv) = isVarAssignable(var, venv)

  fun transTy (tenv, A.NameTy(symbol, pos)) =
      (case S.look(tenv, symbol)
        of SOME ty => ty
         | NONE => (ErrorMsg.error pos ("type not found: " ^ S.name(symbol)); T.UNIT))
    | transTy (tenv, A.ArrayTy(symbol, pos)) =
      (case S.look(tenv, symbol)
        of SOME ty => T.ARRAY(ty, ref ())
         | NONE => (ErrorMsg.error pos ("type not found: " ^ S.name(symbol)); T.UNIT))
    | transTy (tenv, A.RecordTy(fieldList)) =
      T.RECORD(map
                (fn field => (#name field, transTy(tenv, A.NameTy(#typ field, #pos field))))
                fieldList,
              ref ())

  fun transExp(venv, tenv, level) =
    let
      fun trexp (A.OpExp{left, oper, right, pos}) =
          let
            val {exp=expLeft, ty=tyLeft} = trexp left
            val {exp=expRight, ty=tyRight} = trexp right
            fun verifyArithOperands(binop) =
              (checkInt(tyLeft, pos);
               checkInt(tyRight, pos);
               {exp=IR.Ex(Tree.BINOP(binop, IR.unEx(expLeft), IR.unEx(expRight))), ty=T.INT})
            fun verifyEquatableOperands(relop) =
              (checkEqualOrThrow(tyLeft, tyRight, pos);
               {exp=IR.Cx(fn (t, f) => Tree.CJUMP(relop, IR.unEx(expLeft), IR.unEx(expRight), t, f)), ty=T.INT})
            fun verifyComparableOperands(relop) =
              ((if (tyLeft = T.STRING andalso tyRight = T.STRING) orelse (tyLeft = T.INT andalso tyRight = T.INT)
                then ()
                else ErrorMsg.error pos "comparable types must be string or int");
               {exp=IR.Cx(fn (t, f) => Tree.CJUMP(relop, IR.unEx(expLeft), IR.unEx(expRight), t, f)), ty=T.INT})
          in
            case oper
              of A.PlusOp   => verifyArithOperands(Tree.PLUS)
               | A.MinusOp  => verifyArithOperands(Tree.MINUS)
               | A.TimesOp  => verifyArithOperands(Tree.MUL)
               | A.DivideOp => verifyArithOperands(Tree.DIV)
               | A.LtOp     => verifyComparableOperands(Tree.LT)
               | A.LeOp     => verifyComparableOperands(Tree.LE)
               | A.GtOp     => verifyComparableOperands(Tree.GT)
               | A.GeOp     => verifyComparableOperands(Tree.GE)
               | A.EqOp     => verifyEquatableOperands(Tree.EQ)
               | A.NeqOp    => verifyEquatableOperands(Tree.NE)
          end
        | trexp (A.LetExp{decs, body, pos}) =
            let val {venv = venv', tenv = tenv'} = transDecs(venv, tenv, level, decs)
            in
              transExp(venv', tenv', level) body
            end
        | trexp (A.VarExp(var)) = trvar(var)
        | trexp (A.NilExp) = {exp = IR.Ex(Tree.TODO), ty = T.NIL}
        | trexp (A.IntExp(n)) = {exp = IR.Ex(Tree.CONST n), ty = T.INT}
        | trexp (A.StringExp(str, posn)) = {exp = IR.Ex(Tree.TODO), ty = T.STRING}
        | trexp (A.CallExp{func, args, pos}) =
          (case S.look(venv, func)
              of SOME(Env.FunEntry{level, label, formals, result}) =>
                (let fun verifyFormals(firstFormal::restFormals, firstArg::restArgs) =
                         let val firstArgExp = trexp firstArg
                         in
                            if checkEqual(firstFormal, actual_ty(#ty firstArgExp), pos)
                            then verifyFormals(restFormals, restArgs)
                            else ErrorMsg.error pos ("type mismatch in function params: " ^ T.toString(firstFormal) ^ " and " ^ T.toString(#ty firstArgExp))
                         end
                       | verifyFormals(nil, nil) = ()
                       | verifyFormals(_, _) = ErrorMsg.error pos "function formals length differs from arg length"
                 in
                   verifyFormals(formals, args)
                 end;
                {exp = IR.Ex(Tree.TODO), ty = result})
              | SOME _ => (ErrorMsg.error pos "environment entry is not a fun entry";
                           {exp = IR.Ex(Tree.TODO), ty = T.UNIT})
              | NONE => (ErrorMsg.error pos ("undefined function " ^ S.name(func));
                         {exp = IR.Ex(Tree.TODO), ty = T.UNIT}))
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
                              if checkEqual(tyField, ty, pos)
                              then ()
                              else searchFields(rest)
                            end
                         | NONE => ErrorMsg.error pos "record field was undeclared")
                   | searchFields(nil) = ()
              in
                (searchFields fields;
                 {exp = IR.Ex(Tree.TODO), ty = T.RECORD(fieldList, unique)})
              end
            | _ => (ErrorMsg.error pos "record type was undeclared";
                       {exp=IR.Ex(Tree.TODO), ty=T.UNIT}))
        | trexp (A.SeqExp(exprs)) =
          let fun verifyExprs nil = ({exp = IR.Ex(Tree.TODO), ty = T.UNIT})
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
                (checkEqualOrThrow(exprTy, varTy, pos);
                 {exp = IR.translateAssign(varExp, exprExp), ty = T.UNIT})
              end
          else (ErrorMsg.error pos "cannot re-assign to var"; {exp = IR.Ex(Tree.TODO), ty = T.UNIT})
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
                  (checkEqualOrThrow(tyThen, tyElse, pos);
                   {exp = IR.Ex(Tree.TODO), ty = tyThen})
                end
              | NONE => (checkUnit(tyThen, pos);
                         {exp = IR.Ex(Tree.TODO), ty = T.UNIT}))
          end
        | trexp (A.WhileExp{test, body, pos}) =
          let
            val {exp=expTest, ty=tyTest} = trexp test
            val tenvUpdated = S.enter(tenv, breakable, T.UNIT)
            val {exp=expBody, ty=tyBody} = (transExp(venv, tenvUpdated, level) body)
          in
            checkInt(tyTest, pos);
            checkUnit(tyBody, pos);
            {exp = IR.Ex(Tree.TODO), ty = T.UNIT}
          end
        | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
          let
            val {exp=loExp, ty=tyLo} = trexp lo
            val {exp=hiExp, ty=tyHi} = trexp hi
            val access = IR.allocLocal level (!escape)
            val venvUpdated = S.enter(venv, var, Env.ReadVarEntry{access=access, ty=T.INT})
            val tenvUpdated = S.enter(tenv, breakable, T.UNIT)
            val {exp=updatedExp, ty=updatedTy} = (transExp(venvUpdated, tenvUpdated, level) body)
          in
            checkInt(tyLo, pos);
            checkInt(tyHi, pos);
            checkUnit(updatedTy, pos);
            {exp=IR.Ex(Tree.TODO), ty=T.UNIT}
          end
        | trexp (A.BreakExp(pos)) = (checkCanBreak(tenv, pos); {exp = IR.Ex(Tree.TODO), ty =  T.UNIT})
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
                      checkEqualOrThrow(actual_ty(ty), actual_ty(tyInit), pos);
                      {exp = IR.Ex(Tree.TODO), ty = T.ARRAY(ty, unique)})
                   | _ => (ErrorMsg.error pos ("type is not array " ^ S.name(typ));
                          {exp = IR.Ex(Tree.TODO), ty = T.UNIT}))
              | NONE => (ErrorMsg.error pos ("undefined array type " ^ S.name(typ));
                         {exp = IR.Ex(Tree.TODO), ty = T.UNIT}))
          end

      and trvar (A.SimpleVar(id, pos)) =
            (case S.look(venv, id)
                of SOME(Env.VarEntry{access, ty}) =>
                   {exp = IR.Ex(Tree.TODO), ty = actual_ty ty}
                 | SOME(Env.ReadVarEntry{access, ty}) =>
                    {exp = IR.Ex(Tree.TODO), ty = actual_ty ty}
                 | SOME _ => (ErrorMsg.error pos "environment entry is not a var entry";
                              {exp = IR.Ex(Tree.TODO), ty = T.UNIT})
                 | NONE => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
                            {exp = IR.Ex(Tree.TODO), ty = T.UNIT}))
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
                {exp=IR.Ex(Tree.TODO), ty = getFieldTypeWithId(fields, id, pos)}
              | _ => (ErrorMsg.error pos "tried to access record field of object that is not a record";
                     {exp=IR.Ex(Tree.TODO), ty = T.UNIT}))
          end
        | trvar (A.SubscriptVar(var, exp, pos)) =
          let
            val {exp=expVar, ty=tyVar} = trvar(var)
          in
            case tyVar
              of T.ARRAY (ty, unique) => {exp=IR.Ex(Tree.TODO), ty=ty}
               | _ => (ErrorMsg.error pos ("Attempted to access a non-array type: " ^ T.toString(tyVar));
                      {exp=IR.Ex(Tree.TODO), ty=T.UNIT})
          end
    in
      trexp
    end

  and transDec (venv, tenv, level, A.VarDec{name, escape, typ, init, pos}) =
      let
        val {exp, ty=tyInit} = transExp(venv, tenv, level) init
        val access = IR.allocLocal level (!escape)
      in
        (case typ
          of SOME ty =>
             let
               val tyResult = transTy(tenv, A.NameTy(ty))
             in (checkEqualOrThrow(tyInit, tyResult, pos);
                 {tenv = tenv, venv = S.enter(venv, name, Env.VarEntry{access = access, ty = tyResult})})
             end
           | NONE => (if (tyInit = T.NIL)
                      then (ErrorMsg.error pos "Cannot assign expression to nil")
                      else ();
                      {tenv = tenv, venv = S.enter(venv, name, Env.VarEntry{access = access, ty = tyInit})}))
      end
    | transDec (venv, tenv, level, A.TypeDec(typeDecls)) = transTypeDecls(venv, tenv, typeDecls)
    | transDec (venv, tenv, level, A.FunctionDec(functionDecls)) = transFuncDecls(venv, tenv, level, functionDecls)

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

  and transFuncDecls (venv, tenv, level, functionDecls) =
    let
      fun transparam{name, escape, typ, pos} =
        (case S.look(tenv, typ)
          of SOME t => {name=name, ty=t}
           | NONE => {name=name, ty=T.UNIT})

      fun verifyUnique({name, params, body, pos, result}, visited) =
        if contains(visited, name)
        then (ErrorMsg.error pos "multiple matching function names in function declaration sequence"; visited)
        else name::visited

      fun resultTy(result) =
          (case result
            of SOME(returnTy, returnPos) =>
              let val result_ty = S.look(tenv, returnTy)
              in
                (case result_ty
                  of SOME t => t
                   | NONE => (ErrorMsg.error returnPos "type not found"; T.UNIT))
              end
             | NONE => T.UNIT)

      fun buildBaseVenv ({name, params, body, pos, result}, venv) =
        let
          val escapeList = map (fn {name, escape, typ, pos} => !escape) params
          val newlevel = IR.newLevel{parent=level, name=Temp.newlabel(), formals=escapeList}
          val funEntry = Env.FunEntry{level = newlevel,
                                      label = Temp.newlabel(),
                                      formals= map #ty (map transparam params),
                                      result=resultTy result}
        in
          S.enter(venv, name, funEntry)
        end
      val dummyVenv = foldl buildBaseVenv venv functionDecls

      fun verifyReturnType({name, params, body, pos, result}, {venv, tenv}) =
        let
          val newlevel =
            (case S.look(dummyVenv, name)
              of SOME (Env.FunEntry{level, label, formals, result}) => level
                 | _ => (ErrorMsg.error pos "function not found"; level)) (* Error should not occur! *)
          val paramsAndFormals = ListPair.zip(params, IR.formals newlevel)
          val params' = map (fn (param, access) =>
                              let
                                val {name=name, ty=ty} = transparam param
                              in
                                {name=name,ty=ty,access=access}
                              end)
                            paramsAndFormals
          fun enterparam ({name, ty, access}, venv) =
            S.enter(venv, name, Env.VarEntry{access=access, ty=ty})
          val venvAndFunEntry = S.enter(venv, name, Env.FunEntry{formals = map #ty params', result = resultTy(result), level = newlevel, label = Temp.newlabel()})
          val returnVenv = foldl enterparam venvAndFunEntry params'
          val {exp=funExp, ty=funTy} = transExp(returnVenv, tenv, newlevel) body
        in
          checkEqualOrThrow(funTy, resultTy(result), pos);
          {venv=venvAndFunEntry, tenv=tenv}
        end

    in
      foldl verifyUnique nil functionDecls;
      foldl verifyReturnType {venv=dummyVenv, tenv=tenv} functionDecls
    end

  and transDecs (venv, tenv, level, decs) =
    let
      fun f({ve, te}, nil) = (ErrorMsg.error 0 "empty declaration list"; {venv=venv, tenv=tenv}) (* NOTE should never occur *)
        | f({ve, te}, dec::nil) = transDec(ve, te, level, dec)
        | f({ve, te}, dec::decs) =
          let val {venv=venv', tenv=tenv'} = transDec(ve, te, level, dec)
          in
            f({ve=venv', te=tenv'}, decs)
          end
    in
      f ({ve=venv, te=tenv}, decs)
    end

  fun transProg absyn =
    let
      val mainLevel = IR.newLevel{parent=IR.outermost, name=Temp.newlabel(), formals=[]}
      val _ = FindEscape.findEscape(absyn)
      val {exp, ty} = transExp(Env.base_venv, Env.base_tenv, mainLevel) absyn
    in
      exp
    end
end
