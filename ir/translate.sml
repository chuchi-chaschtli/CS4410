structure A = Absyn
structure F = Frame

signature TRANSLATE =
sig
    datatype exp = Ex of Tree.exp
                 | Nx of Tree.stm
                 | Cx of Temp.label * Temp.label -> Tree.stm
    type level
    type access (* Not same as Frame.access *)

    val outermost : level
    val newLevel : {parent:level, name:Temp.label, formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access

    val zero : Tree.exp
    val one : Tree.exp

    val translateInt    : int -> exp
    val translateNil    : unit -> exp
    val translateArith  : Absyn.oper * exp * exp -> exp
    val translateRelop  : Absyn.oper * exp * exp -> exp
    val translateFor    : exp * bool ref * exp * exp * exp * Temp.label -> exp
    val translateWhile  : exp * exp * Tree.label -> exp
    val translateBreak  : Tree.label             -> exp
    val translateIf     : exp * exp * exp        -> exp
    val translateAssign : exp * exp              -> exp
    val translateCall   : level * level * Tree.label * exp list -> exp
    val translateSeqExp : exp list -> exp

    val translateSimpleVar    : access * level -> exp
    val translateFieldVar     : exp * int -> exp
    val translateSubscriptVar : exp * exp -> exp
    val initArray: exp * exp -> exp
    val initRecord: exp list -> exp

    val translateVarDec : access   * exp -> exp
    val translateLet    : exp list * exp -> exp

    val dummyOp: unit -> exp

    val unEx : exp -> Tree.exp
    val unNx : exp -> Tree.stm
    val unCx : exp -> Temp.label * Temp.label -> Tree.stm

    val procEntryExit : {level: level, body: exp} -> unit
    val getResult : unit -> F.frag list
end

structure Translate : TRANSLATE =
struct
  type exp = unit
  datatype level = LEVEL of {frame: F.frame, parent: level}
                 | GLOBAL (* base-case being the top level *)
  type access = level * F.access

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm

  val outermost = GLOBAL
  val zero = Tree.CONST 0
  val one = Tree.CONST 1
  val word = Tree.CONST(F.wordSize)

  val fragList : F.frag list ref = ref nil

  fun newLevel {parent=parent, name=name, formals=formals} =
    LEVEL{frame=F.newFrame({name=name, formals=true::formals}), parent=parent}

  fun formals level =
    (case level
      of LEVEL {frame, parent} => let
                                    val formals = F.formals frame
                                  in
                                    map (fn access => (level, access)) formals
                                  end
       | GLOBAL => nil)

  (* Allocate local access for a given level *)
  fun allocLocal level escape =
    (case level
      of LEVEL {frame, parent} => let val f_access = F.allocLocal frame escape
                                  in
                                    (level, f_access)
                                  end)

  (*
    A no-op that can be optimized away as a register->register move during
    register allocation
  *)
  fun dummyOp() =
    let val tmp = Temp.newtemp()
        val exp = Tree.TEMP(tmp)
    in Nx (Tree.MOVE (exp, exp))
    end

  (* Builds a SEQ from a list of expressions as a convenience function *)
  fun buildSeq(first::nil) = first
    | buildSeq(first::rest) = Tree.SEQ(first, buildSeq rest)

  (* chases static links by recursing up the usage level until we reach the
  declaration level, or error otherwise *)
  fun traverseStaticLinks(dec, use) =
    if use = dec
    then Tree.TEMP(F.FP)
    else case use
      of LEVEL {frame, parent} => Tree.MEM (traverseStaticLinks(dec, parent)) (* NOTE our static link offset is 0 *)
       | GLOBAL => (ErrorMsg.error 0 "Cannot find any static links"; Tree.TODO)

  fun unEx (Ex e) = e
    | unEx (Cx genstm) =
			let
				val r = Temp.newtemp()
				val t = Temp.newlabel()
        val f = Temp.newlabel()
			in
        Tree.ESEQ(
          buildSeq([
              Tree.MOVE(Tree.TEMP r, one),
              genstm(t,f),
              Tree.LABEL f,
              Tree.MOVE(Tree.TEMP r, zero),
              Tree.LABEL t
            ]),
					Tree.TEMP r)
			end
    | unEx (Nx s) = Tree.ESEQ(s, zero)

  fun unCx (Cx c) = c
    | unCx (Ex e) = (if e = zero
                     then (fn(tlabel, flabel) => Tree.JUMP(Tree.NAME(flabel), [flabel]))
                     else if e = one
                          then fn(tlabel, flabel) => Tree.JUMP(Tree.NAME(tlabel), [tlabel])
                          else fn(tlabel, flabel) => Tree.CJUMP(Tree.EQ, one, e, tlabel, flabel))
    | unCx (Nx _) = (ErrorMsg.error 0 "Cannot process no-result on conditional";
                     fn (a, b) => Tree.LABEL(Temp.newlabel()))

  fun unNx (Ex e) = Tree.EXP(e)
    | unNx (Nx n) = n
    | unNx (c)    = unNx(Ex(unEx(c)))

  fun convertBinop A.PlusOp = Tree.PLUS
    | convertBinop A.MinusOp = Tree.MINUS
    | convertBinop A.TimesOp = Tree.MUL
    | convertBinop A.DivideOp = Tree.DIV
    | convertBinop _ = (ErrorMsg.error 0 "Unsupported binop conversion"; Tree.DIV)

  fun convertRelop A.LtOp = Tree.LT
    | convertRelop A.LeOp = Tree.LE
    | convertRelop A.GtOp = Tree.GT
    | convertRelop A.GeOp = Tree.GE
    | convertRelop A.EqOp = Tree.EQ
    | convertRelop A.NeqOp = Tree.NE
    | convertRelop _ = (ErrorMsg.error 0 "Unsupported relop conversion"; Tree.NE)

  fun translateInt(n) = Ex (Tree.CONST n)

  fun translateNil() = Ex zero

  fun translateArith(oper, left, right) =
    Ex (Tree.BINOP (convertBinop oper, unEx left, unEx right))

  fun translateRelop(oper, left, right) =
    Cx (fn (t, f) => Tree.CJUMP(convertRelop oper, unEx left, unEx right, t, f))

  (*
    init: var <- lo
          CJUMP (<= var hi) body break
    body: BODY
          CJUMP (< var hi) loop break
    loop: var <- var + 1
          JMP body
    break:
  *)
  fun translateFor(var, escape, lo, hi, body, breakTmp) =
    let
      val var = unEx var
      val hi = unEx hi
      val bodyTmp = Temp.newlabel()
      val loopTmp = Temp.newlabel()
      val initialSeqs = [
        Tree.MOVE(var, unEx lo),
        Tree.CJUMP(Tree.LE, var, hi, bodyTmp, breakTmp)
      ]
      val bodySeqs = [
        Tree.LABEL(bodyTmp),
        unNx body,
        Tree.CJUMP(Tree.LT, var, hi, loopTmp, breakTmp)
      ]
      val loopSeqs = [
        Tree.LABEL(loopTmp),
        Tree.MOVE(var, Tree.BINOP(Tree.PLUS, var, one)),
        Tree.JUMP(Tree.NAME(bodyTmp), bodyTmp::nil),
        Tree.LABEL(breakTmp)
      ]
    in
      Nx(buildSeq(initialSeqs @ bodySeqs @ loopSeqs))
    end

  (*
    test: CJMP TEST body break
    body: BODY
          JMP test
    break:
  *)
  fun translateWhile(test, body, breakTmp) =
    let val testTmp = Temp.newlabel()
        val bodyTmp = Temp.newlabel()
    in
      Nx(
        buildSeq([
          Tree.LABEL testTmp,
          unCx test (bodyTmp, breakTmp),
          Tree.LABEL bodyTmp,
          unNx body,
          Tree.JUMP (Tree.NAME testTmp, [testTmp]),
          Tree.LABEL breakTmp
        ]))
    end

  fun translateBreak breakTmp = Nx(Tree.JUMP (Tree.NAME breakTmp, breakTmp::nil))

  (*
          CJMP TEST then else
    then: THEN
          JMP done
    else: ELSE
    done:
  *)
  (* TODO should this be returning Nx? "if" should return a value IIRC *)
  fun translateIf(testExp, thenExp, elseExp) =
    let
      val thenTmp = Temp.newlabel()
      val elseTmp = Temp.newlabel()
      val resultTmp = Temp.newtemp()
      val joinTmp = Temp.newlabel()
    in
    Ex(
      Tree.ESEQ(
        buildSeq([
          unCx testExp (thenTmp, elseTmp),
          Tree.LABEL(thenTmp),
          Tree.MOVE(Tree.TEMP(resultTmp), unEx thenExp),
          Tree.JUMP(Tree.NAME(joinTmp), [joinTmp]),
          Tree.LABEL(elseTmp),
          Tree.MOVE(Tree.TEMP(resultTmp), unEx elseExp),
          Tree.JUMP(Tree.NAME(joinTmp), [joinTmp])]),
        Tree.TEMP(resultTmp)
      )
    )
    end

  fun translateAssign(v, e) = Nx (Tree.MOVE (unEx v, unEx e))

  fun translateCall(dec, call, label, args) =
    case dec
      of GLOBAL => Ex (Tree.TEMP(F.FP))
       | LEVEL {frame, parent} =>
        let val link = traverseStaticLinks(parent, call)
            val processedArgs = map unEx args
        in Ex (Tree.CALL (Tree.NAME label, link::processedArgs))
        end

  fun translateSeqExp(exps) =
    let
      fun helper(nil, nil) = dummyOp()
        | helper(exp::exps, acc) = helper(exps, exp::acc)
        | helper(nil, acc::accs) =
          let val stms = map unNx (rev(accs))
          in Ex (Tree.ESEQ(buildSeq(stms), unEx acc))
          end
    in
      helper(exps, nil)
    end

  fun calculateAddress(ex, index) =
    Tree.BINOP(Tree.PLUS, ex, Tree.BINOP(Tree.MUL, index, word))

  fun translateSimpleVar ((dec, access), use) =
    Ex(F.exp access (traverseStaticLinks(dec, use)))

  fun translateFieldVar(name, element) =
    Ex(Tree.MEM(calculateAddress(unEx name, Tree.CONST(element))))

  fun translateSubscriptVar(array, index) =
    let val tmp = Temp.newtemp()
    in
      Ex(Tree.ESEQ(
          Tree.MOVE(Tree.TEMP(tmp), calculateAddress(unEx array, unEx index)),
          Tree.MEM(Tree.TEMP(tmp))))
    end

  fun initArray(size, init) =
    Ex(F.externalCall("initArray", [unEx size, unEx init]))

  fun initRecord (exps) =
    let
      val exps' = map unEx exps
      val r = Temp.newtemp()
      fun initFields(exp::exps, value) =
        let
          val move = Tree.MOVE(Tree.MEM
                                (Tree.BINOP(Tree.PLUS, Tree.TEMP r, Tree.CONST (value * F.wordSize))),
                                exp)
        in move::initFields(exps, value + 1)
        end
      val seqs = buildSeq(initFields(exps', 0))
    in
      Ex (Tree.ESEQ
          (Tree.SEQ(Tree.MOVE (Tree.TEMP r,
             F.externalCall("initRecord", [Tree.CONST (length(exps) * F.wordSize)])), (* TODO: replace with malloc? *)
           seqs),
           Tree.TEMP r))
    end

  fun translateVarDec((level, access), valExp) = (Nx(Tree.MOVE(F.exp(access)(Tree.TEMP(F.FP)), unEx valExp)))

  fun translateLet(assignments, body) =
    let val assignments' = map unNx assignments
    in
      Ex (Tree.ESEQ(buildSeq(assignments'), unEx body))
    end

  (* TODO Properly send return value to F.RV *)
  (* TODO does the order here matter? *)
  fun procEntryExit({level: level, body: exp}) =
    (case level
      of LEVEL{frame, parent} => let val retExp = Tree.MOVE(Tree.TEMP(F.RV), unEx body)
                                     val post1Body = F.procEntryExit1(frame, retExp)
                                 in
                                   let val post3Body = F.procEntryExit3(frame, post1Body)
                                   in
                                     fragList := F.PROC{body=post3Body, frame=frame} :: !fragList
                                   end
                                 end
       | GLOBAL => ())

  fun getResult() = !fragList (* TODO ref to frag list within Translate*)
end
