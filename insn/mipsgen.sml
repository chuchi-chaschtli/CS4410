signature CODEGEN =
sig
    structure Frame: FRAME
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct

structure T = Tree
structure A = Assem
structure Frame = MipsFrame

fun codegen frame stm =
  let
    val ilist = ref nil
    fun emit x = ilist := x :: !ilist
	  fun result(gen) = let val tmp = Temp.newtemp()
                      in (gen tmp; tmp)
                      end

    fun binop T.PLUS  = "add"
      | binop T.MINUS = "sub"
      | binop T.DIV   = "div"
      | binop T.MUL   = "mul"
      | binop _       = ErrorMsg.impossible "invalid binop supplied"

    fun relop(oper, zero) =
      let
        val ending = if zero then "z" else ""
        fun relop' T.EQ = "beq" ^ ending
          | relop' T.NE = "bne" ^ ending
          | relop' T.GE = "bge" ^ ending
          | relop' T.GT = "bgt" ^ ending
          | relop' T.LE = "ble" ^ ending
          | relop' T.LT = "blt" ^ ending
          | relop' T.ULT = "bltu"
          | relop' T.UGT = "bgtu"
          | relop' T.ULE = "bleu"
          | relop' T.UGE = "bgeu"
      in
        if zero andalso (oper = T.ULT orelse oper = T.UGT orelse oper = T.ULE orelse oper = T.UGE)
        then ErrorMsg.impossible "invalid binop comparison to 0 supplied"
        else relop' oper
      end

    fun munchStm (T.SEQ(x, y)) = (munchStm x; munchStm y)
      | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST n)), e2)) =
        emit (A.OPER {assem="sw 's1, "^ Int.toString n ^ "('s0)\n",
                      src=[munchExp e1, munchExp e2],
                      dst=nil,
                      jump=NONE})
      | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST n, e1)), e2)) =
        emit (A.OPER {assem="sw 's1, " ^ Int.toString n ^ "('s0)\n",
                      src=[munchExp e1, munchExp e2],
                      dst=nil,
                      jump=NONE})
      | munchStm (T.MOVE(T.MEM(T.CONST n), e)) =
        emit (A.OPER {assem="sw 's0, " ^ Int.toString n ^ "($r0)\n",
                      src=[munchExp e],
                      dst=nil,
                      jump=NONE})
      | munchStm (T.MOVE(T.MEM(e1), e2)) =
        emit (A.OPER {assem="sw 's1, 0('s0)\n",
                      src=[munchExp e1, munchExp e2],
                      dst=nil,
                      jump=NONE})
      | munchStm(T.MOVE(T.ESEQ(s, T.MEM(e1)), e2)) =
        (munchStm s;
         emit (A.OPER {assem="sw `s0, (`d0)\n",
                       src=[munchExp e2],
                       dst=[munchExp e1],
                       jump=NONE}))
      | munchStm(T.MOVE(T.ESEQ(s, T.TEMP(t)), e)) =
        (munchStm s;
         emit (A.MOVE {assem="move `d0, `s0\n",
                       src=munchExp e,
                       dst=t}))
      | munchStm(T.MOVE(T.TEMP t1, T.TEMP t2)) =
        emit (A.MOVE{assem="",
                     src=t2,
			               dst=t1})
      | munchStm (T.MOVE(T.TEMP(t), T.CALL(e, args))) =
        emit (A.OPER{assem="jal `s0\nmove " ^ Frame.tempToString(t) ^ ", `d0\n",
                     src=munchExp(e)::munchArgs(0, args),
                     dst=[Frame.RA, Frame.RV]@Frame.calleesaves,
                     jump=NONE})
      | munchStm (T.MOVE(T.TEMP i, e)) =
        emit (A.OPER {assem="move 'd0, 's0\n",
                      src=[munchExp e],
                      dst=[i],
                      jump=NONE})
      | munchStm (T.JUMP(T.NAME l, labels)) =
        emit (A.OPER {assem="j 'j0\n",
                      src=nil,
                      dst=nil,
                      jump=SOME labels})
      | munchStm (T.CJUMP (oper, (T.CONST 0), e, t, f)) =
        munchStm (T.CJUMP (Tree.commute(oper), e, (T.CONST 0), t, f))
      | munchStm (T.CJUMP (oper, e, (T.CONST 0), t, f)) = (* For comparisons where the content of a register is zero *)
        emit (A.OPER {assem=relop(oper, true) ^ " `s0, `j0\n",
                      src=[munchExp e],
                      dst=nil,
                      jump=SOME [t,f]})
      | munchStm (T.CJUMP (oper, e1, e2, t, f)) =
        emit (A.OPER {assem=relop(oper, false) ^ " `s0, `s1, `j0\n" ,
                      src=[munchExp e1, munchExp e2],
                      dst=nil,
                      jump=SOME [t,f]})
      | munchStm (T.LABEL label) =
        emit (A.LABEL {assem=Symbol.name(label) ^ ":\n", lab=label})
      | munchStm(T.EXP e) = (munchExp e; ())
      | munchStm stm =
        (Printtree.printtree(TextIO.stdOut, stm);
         ErrorMsg.impossible "Could not munch statement")

    and munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST n, e))) =
        result(fn register =>
          emit(A.OPER
               {assem="lw 'd0, " ^ Int.toString n ^ "('s0)\n",
                src=[munchExp e],
                dst=[register],
                jump=NONE}))
      | munchExp(T.MEM(T.BINOP(T.PLUS, e, T.CONST n))) =
	      result(fn register =>
          emit(A.OPER
               {assem="lw 'd0, " ^ Int.toString n ^"('s0)\n",
                src=[munchExp e],
                dst=[register],
                jump=NONE}))
      | munchExp(T.BINOP(T.PLUS, e, T.CONST n)) =
        result(fn register =>
          emit(A.OPER
               {assem="addi 'd0, 's0, " ^ Int.toString n,
                src=[munchExp e],
                dst=[register],
                jump=NONE}))
      | munchExp(T.BINOP(T.PLUS, T.CONST n, e)) =
        result(fn register =>
          emit(A.OPER
               {assem="addi 'd0, 's0, " ^ Int.toString n,
                src=[munchExp e],
                dst=[register],
                jump=NONE}))
      | munchExp(T.BINOP(T.MINUS, e, T.CONST n)) =
        result(fn register =>
          emit(A.OPER
               {assem="addi 'd0, 's0, " ^ Int.toString (~n),
                src=[munchExp e],
                dst=[register],
                jump=NONE}))
      | munchExp(T.BINOP(oper, e1, e2)) =
        result(fn register =>
          emit(A.OPER
               {assem=binop(oper) ^ " 'd0, 's0, 's1",
                src=[munchExp e1, munchExp e2],
                dst=[register],
                jump=NONE}))
      | munchExp(T.MEM(T.CONST n)) =
        result(fn register =>
          emit(A.OPER
               {assem="lw 'd0, " ^ Int.toString n ^ "($r0)\n",
                src=nil,
                dst=[register],
                jump=NONE}))
      | munchExp(T.MEM(e)) =
        result(fn register =>
          emit(A.OPER
               {assem="lw 'd0, 0('s0)",
                src=[munchExp e],
                dst=[register],
                jump=NONE}))
      | munchExp(T.CONST n) =
        result(fn register =>
          emit(A.OPER
               {assem="li 'd0, " ^ Int.toString n,
               src=nil,
               dst=[register],
               jump=NONE}))
      | munchExp(T.NAME n) =
        result(fn register =>
          emit(A.OPER
               {assem="la `d0, " ^ Symbol.name n ^ "\n",
                src=nil,
                dst=[register],
                jump=NONE}))
      | munchExp(T.TEMP temp) = temp
      | munchExp(T.ESEQ(s, e)) = (munchStm s; munchExp e)
      | munchExp(T.CALL(e, args)) =
        result(fn register =>
          emit(A.OPER{assem="jal `s0\n",
                      src=munchExp(e)::munchArgs(0, args),
                      dst=[Frame.RA, Frame.RV]@Frame.calleesaves,
                      jump=NONE}))

    and munchArgs(i, nil) = nil
      | munchArgs(i, arg::args) =
        let
          val onRegister = i < Frame.numDedicatedArgRegisters
          val temp = List.nth (Frame.argregs, i)
          fun tempFromArg arg = munchStm (T.MOVE(T.TEMP(temp), arg))
          fun frameFromArg arg =
            let
              val offset = ~(i - Frame.numDedicatedArgRegisters)*Frame.wordSize
            in
              munchStm(T.MOVE(T.BINOP(T.PLUS, T.TEMP(Frame.SP), T.CONST offset), arg))
            end
        in
          if onRegister
          then (tempFromArg arg; temp::munchArgs(i + 1, args))
          else (frameFromArg arg; munchArgs(i + 1, args))
        end
  in
	 (munchStm stm; rev(!ilist))
  end
end
