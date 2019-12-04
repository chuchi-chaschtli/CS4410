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

  type register = string
  fun createTempMap() =
    let
      val map : (register Temp.Table.table) = Temp.Table.empty
      val map = Temp.Table.enter(map, FP, "$fp")
      (* TODO is the Return Value register correct? *)
      (* https://course.ccs.neu.edu/csu4410/spim_documentation.pdf *)
      val map = Temp.Table.enter(map, RV, "$v0")
      val map = Temp.Table.enter(map, SP, "$sp")
      val map = Temp.Table.enter(map, RA, "$ra")
      val map = Temp.Table.enter(map, ZERO, "$zero")
    in
      map
    end
  val tempMap = createTempMap();
  fun tempToString(temp) =
    case Temp.Table.look(tempMap, temp)
    of SOME(name) => name
     | NONE       => Temp.makestring(temp)

  val specialregs = [FP,RV,SP,RA,ZERO]
  val argregs = getTemps(4)
  val calleesaves = getTemps(8)
  val callersaves = getTemps(10)
  val registerTemps = specialregs@argregs@calleesaves@callersaves

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
