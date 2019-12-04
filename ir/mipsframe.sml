signature FRAME =
sig
  type access
  type frame

  val FP: Temp.temp (* frame pointer *)
  val RV: Temp.temp (* return value *)
  val SP: Temp.temp (* static pointer *)
  val RA: Temp.temp (* return address *)
  val ZERO: Temp.temp (* zero register *)

  type register
  val tempMap: register Temp.Table.table
  val tempToString: Temp.temp -> string

  val specialregs: Temp.temp list
  val argregs: Temp.temp list
  val calleesaves: Temp.temp list
  val callersaves: Temp.temp list
  val registerTemps: Temp.temp list
  val registerTempNames: register list

  val wordSize: int
  val numDedicatedArgRegisters: int

  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name: frame -> Temp.label
  val formals : frame -> access list

  val allocLocal : frame -> bool -> access
  val externalCall: string * Tree.exp list -> Tree.exp
  val exp: access -> Tree.exp -> Tree.exp

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
  val procEntryExit3 : frame * Assem.instr list -> {prolog: string, body: Assem.instr list, epilog: string}
end

structure MipsFrame : FRAME =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name: Temp.label, frameOffset: int ref, formals: access list}

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  fun getTempsHelper(0, acc) = acc
    | getTempsHelper(n, acc) = getTempsHelper(n - 1, Temp.newtemp()::acc)

  fun getTemps(n) = getTempsHelper(n, nil)

  val FP = Temp.newtemp()
  val RV = Temp.newtemp()
  val SP = Temp.newtemp()
  val RA = Temp.newtemp()
  val ZERO = Temp.newtemp()
  val specialregs = [FP,RV,SP,RA,ZERO]
  val argregs = getTemps(4)
  val calleesaves = getTemps(8)
  val callersaves = getTemps(10)

  type register = string
  fun createTempMap() =
    let
      (* https://course.ccs.neu.edu/csu4410/spim_documentation.pdf *)
      val registerNames = [(FP, "$fp"),
                           (RV, "$v0"),
                           (SP, "$sp"),
                           (RA, "$ra"),
                           (ZERO, "$zero"),
                           (* ARG registers = a0 - a3 *)
                           (List.nth(argregs, 0), "$a0"),
                           (List.nth(argregs, 1), "$a1"),
                           (List.nth(argregs, 2), "$a2"),
                           (List.nth(argregs, 3), "$a3"),
                           (* CALLEE saves = s0 - s7 *)
                           (List.nth(calleesaves, 0), "$s0"),
                           (List.nth(calleesaves, 1), "$s1"),
                           (List.nth(calleesaves, 2), "$s2"),
                           (List.nth(calleesaves, 3), "$s3"),
                           (List.nth(calleesaves, 4), "$s4"),
                           (List.nth(calleesaves, 5), "$s5"),
                           (List.nth(calleesaves, 6), "$s6"),
                           (List.nth(calleesaves, 7), "$s7"),
                           (* CALLER saves = t0 - t9 *)
                           (List.nth(callersaves, 0), "$t0"),
                           (List.nth(callersaves, 1), "$t1"),
                           (List.nth(callersaves, 2), "$t2"),
                           (List.nth(callersaves, 3), "$t3"),
                           (List.nth(callersaves, 4), "$t4"),
                           (List.nth(callersaves, 5), "$t5"),
                           (List.nth(callersaves, 6), "$t6"),
                           (List.nth(callersaves, 7), "$t7"),
                           (List.nth(callersaves, 8), "$t8"),
                           (List.nth(callersaves, 9), "$t9")]
      val initialMap : (register Temp.Table.table) = Temp.Table.empty
      val map = foldl (fn ((reg, name), tbl) => Temp.Table.enter(tbl, reg, name))
                      initialMap
                      registerNames
    in
      map
    end
  val tempMap = createTempMap();
  fun tempToString(temp) =
    case Temp.Table.look(tempMap, temp)
    of SOME(name) => name
     | NONE       => Temp.makestring(temp)


  val registerTemps = calleesaves@callersaves
  val registerTempNames = foldl (fn (reg, names) => tempToString(reg)::names)
                                nil
                                registerTemps

  val wordSize = 4
  val numDedicatedArgRegisters = 4

  fun newFrame {name, formals} =
    let
      val registersTaken = ref 0
      val paramIndex = ref 1
      fun getParamOffset() =
        let val tmp = !paramIndex
        in
          paramIndex := tmp+1;
          tmp * wordSize
        end
      fun allocateFormal escape =
        (registersTaken := Int.min(!registersTaken + 1, numDedicatedArgRegisters + 1);
          if escape orelse !registersTaken > numDedicatedArgRegisters
          then InFrame (getParamOffset())
          else InReg   (Temp.newtemp()))
      val formals' = map allocateFormal formals
    in
       {name=name, frameOffset=(ref 0), formals=formals'}
    end

  fun allocLocal {name, frameOffset, formals} escape =
      (frameOffset := !frameOffset - wordSize;
       if escape
       then InFrame (!frameOffset)
       else InReg   (Temp.newtemp()))

  fun name (frame:frame) = (#name frame)
  fun formals (frame:frame) = (#formals frame)

  fun externalCall (s, args) = Tree.CALL(Tree.NAME(Temp.namedlabel s), args)

  fun exp (access) fp =
    case access
      of InFrame k => Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST k))
       | InReg temp => Tree.TEMP temp

  fun procEntryExit1(frame, stmt) = stmt (* TODO stub for assignment 5 *)

  fun procEntryExit2(frame, body) =
    body @
      [Assem.OPER{assem="",
                  src=specialregs@calleesaves,
                  dst=[],
                  jump=SOME[]}]

  fun procEntryExit3(frame, body) =
    {prolog = "PROCEDURE " ^ Symbol.name(name(frame)) ^ "\n",
     body = body,
     epilog = "END " ^ Symbol.name(name(frame)) ^ "\n"}
end

structure Frame : FRAME = MipsFrame
