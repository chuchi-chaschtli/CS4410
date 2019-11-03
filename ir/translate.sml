structure F = Frame

signature TRANSLATE =
sig
    type exp
    type level
    type access (* Not same as Frame.access *)

    val outermost : level
    val newLevel : {parent:level, name:Temp.label, formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access
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

  (* Alocate local access for a given level *)
  fun allocLocal level escape =
    (case level
      of LEVEL {frame, parent} => let val f_access = F.allocLocal frame escape
                                  in
                                    (level, f_access)
                                  end)

  (* Builds a SEQ from a list of expressions as a convenience function *)
  fun buildSeq(first::nil) = first
    | buildSeq(first::rest) = Tree.SEQ(first, buildSeqs rest)

  fun unEx (Ex e) = e
    | unEx (Cx genstm) =
			let
				val r = Temp.newtemp()
				val t = Temp.newlabel()
        val f = Temp.newlabel()
			in
        Tree.ESEQ(
          buildSeq([
              Tree.MOVE(Tree.TEMPLOC r, one),
              genstm(t,f),
              Tree.LABEL f,
              Tree.MOVE(Tree.TEMPLOC r, zero),
              Tree.LABEL t
            ]),
					Tree.TEMP r)
			end
    | unEx (Nx s) = Tree.ESEQ(s, zero)

  fun unCx (Cx c)    = c
    | unCx (Ex zero) = (fn(tlabel, flabel) => Tree.JUMP(Tree.NAME(flabel), [flabel]))
    | unCx (Ex one)  = (fn(tlabel, flabel) => Tree.JUMP(Tree.NAME(tlabel), [tlabel]))
    | unCx (Ex e)    = (fn(tlabel, flabel) => Tree.CJUMP(Tree.EQ, one, e, tlabel, flabel))
    | unCx (Nx _)    = (ErrorMsg.error 0 "Cannot process no-result on conditional";
                        fn (a, b) => Tree.LABEL(Temp.newlabel()))

  fun unNx (Ex e) = Tree.EXP(e)
    | unNx (Nx n) = n
    | unNx (c)    = unNx(Ex(unEx(c)))

  (* Converts a temp or memory expression to a location to be used for moves *)
  fun locFromExp (Tree.TEMP t) = Tree.TEMPLOC t
    | locFromExp (Tree.MEM  e) = Tree.MEMLOC e
    | locFromExp Tree.TODO     = (ErrorMsg.error 0 "TODO found"; Tree.TEMPLOC(Temp.newtemp()))
    | locFromExp _             = (ErrorMsg.error 0 "Unable to perform conversion"; Tree.TEMPLOC(Temp.newtemp()))

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

  fun translateAssign(v, e) = Nx (Tree.MOVE (locFromExp (unEx v), unEx e))
end
