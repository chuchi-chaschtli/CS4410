structure FindEscape: sig val findEscape: Absyn.exp -> unit
          end =

struct
  type depth = int
  type escEnv = (depth * bool ref) Symbol.table

  val emptyEnv = Symbol.empty

    (* For a simple variable, check whether the depths match *)
  fun traverseVar(env, d, Absyn.SimpleVar(id, pos)) =
        (case Symbol.look(env, id)
         of SOME(def_depth, escape) => (if d > def_depth
                                        then (escape := true; ())
                                        else ())
          | _ => ()) (* We don't need to error here; undeclared variables are caught via typechecker *)
    (* In field, we only need to check var *)
    | traverseVar(env, d, Absyn.FieldVar(var, id, pos)) = (traverseVar(env, d, var))
    (* In subscript, we can recursively traverse exp and check against var *)
    | traverseVar(env, d, Absyn.SubscriptVar(var, exp, pos)) =
        (traverseVar(env, d, var);
         traverseExp(env, d) exp)

  and traverseExp(env:escEnv, d:depth) =
    let
      fun trexp(Absyn.NilExp) = ()
        | trexp(Absyn.IntExp i) = ()
        | trexp(Absyn.StringExp(str, pos)) = ()
        | trexp(Absyn.BreakExp pos) = ()
        | trexp(Absyn.VarExp(var)) = (traverseVar(env, d, var))
        | trexp(Absyn.CallExp{func, args, pos}) = (map (traverseExp(env, d+1)) args; ())
        | trexp(Absyn.OpExp{left, oper, right, pos}) =
            (trexp left;
             trexp right)
        | trexp(Absyn.RecordExp{fields, typ, pos}) =
            (map (fn (sym, exp, pos) => trexp exp) fields;
             ())
        | trexp(Absyn.SeqExp expSeq) =
            (map (fn (exp, pos) => trexp exp) expSeq;
             ())
        | trexp(Absyn.AssignExp {var,exp,pos}) =
            (trexp exp)
        | trexp(Absyn.IfExp {test, then', else', pos}) =
            (trexp test;
             trexp then';
             case else'
              of SOME stmt => trexp stmt
               | NONE => ())
        | trexp(Absyn.WhileExp {test, body, pos}) =
            (trexp test;
             trexp body)
        | trexp(Absyn.ForExp {var, escape, lo, hi, body, pos}) =
            (escape := false;
             trexp lo;
             trexp hi;
             let
               val env'=Symbol.enter(env, var, (d, escape))
             in
               traverseExp(env', d) body
             end)
        | trexp(Absyn.LetExp{decs, body, pos}) =
            let
              val env' = traverseDecs(env, d, decs)
            in
              traverseExp(env', d) body
            end
        | trexp(Absyn.ArrayExp{typ, size, init, pos}) =
            (trexp size;
             trexp init)
      in
        trexp
      end

  and traverseDecs(env, d, decs: Absyn.dec list) : escEnv =
    let
      fun f(env, d, nil) = env
        | f(env, d, dec::decs) =
          let
            val env' = traverseDec(env, d, dec)
          in
            f(env', d, decs)
          end

      and traverseFunctionDecs(env, d, functionDecs) =
        let
          fun traverseFunctionDec(env, d, {name, params, body, pos, result}) =
            let
              fun traverseField({name, escape, typ, pos}, env) = (escape := false; Symbol.enter(env, name, (d+1, escape)))
              val env' = foldl traverseField env params
            in
              traverseExp(env', d+1) body;
              env'
            end
        in
          (case functionDecs
           of nil => env
            | dec::decs => let
                             val env' = traverseFunctionDec(env, d, dec)
                           in
                             traverseFunctionDecs(env', d, decs)
                           end)
        end

      and traverseDec (env, d, Absyn.VarDec{name, escape, typ, init, pos}) = (escape := false; Symbol.enter(env, name, (d, escape)))
        | traverseDec (env, d, Absyn.TypeDec(typeDecs)) = traverseTypeDecs(env, d, typeDecs)
        | traverseDec (env, d, Absyn.FunctionDec(functionDecs)) = traverseFunctionDecs(env, d, functionDecs)

      and traverseTypeDecs(env, d, typeDecs) = env

    in
      f (env, d, decs)
    end

  fun findEscape(prog: Absyn.exp) : unit = (traverseExp(emptyEnv, 0) prog)
end
