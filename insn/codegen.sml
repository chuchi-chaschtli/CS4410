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

    fun munchStm (T.SEQ(x, y)) =
        (munchStm x;
         munchStm y)
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
	    | munchStm (T.MOVE(T.TEMP i, e)) =
        emit (A.OPER {assem="move 'd0, 's0\n",
                      src=[munchExp e],
                      dst=[i],
                      jump=NONE})
	    | munchStm (T.LABEL label) =
        emit (A.LABEL {assem=Temp.toString(label) ^ ":\n", lab=label})

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
	   | munchExp(T.TEMP temp) = temp
     | munchExp(T.ESEQ(s, e)) = (munchStm s; munchExp e)
  in
	 (munchStm stm; rev(!ilist))
  end
end
