structure FindEscape: sig val findEscape: Absyn.exp -> unit
		      end =

struct
type depth = int
type escEnv = (depth * bool ref) Symbol.table

fun traverseVar(env, d, s: Absyn.var) : unit = ...
and traverseExp(env, d) =
  let
  fun trexp(Absyn.NilExp) = ()
    | trexp(Absyn.IntExp i) = ()
    | trexp(Absyn.StringExp(str, pos)) = ()
    | trexp(Absyn.BreakExp pos) = ()
    | trexp(Absyn.VarExp(var)) = (*TODO: implement var traversal*) ()
    | trexp(Absyn.CallExp{func, args, pos}) = (map trexp args; ())
    | trexp(Absyn.OpExp{left, oper, right, pos}) =
      (trexp left;
       trexp right)
    | trexp(Absyn.RecordExp{fields, typ, pos}) =
      (map
        (fn (sym, exp, pos) => trexp exp)
        fields;
       ())
    | trexp(Absyn.SeqExp expSeq) =
      (map
       (fn (exp, pos) => trexp exp)
       expSeq;
      ())
    | trexp(Absyn.AssignExp {var,exp,pos}) = trexp exp
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
       let val env'=Symbol.enter(env, var, (d, escape))
       in traverseExp(env',d) body
       end)
    | trexp(Absyn.LetExp{decs, body, pos}) =
      let val env' = traverseDecs(env, d) decs
      in traverseExp(env', d) body
      end
    | trexp(Absyn.ArrayExp{typ, size, init, pos}) =
      (trexp size;
       trexp init)
  in
    trexp
  end
and traverseDecs(env, d, s: Absyn.dec list) : escEnv = ...
fun findEscape(prog: Absyn.exp) : unit = ...

end
