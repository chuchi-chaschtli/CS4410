structure Main = struct

structure Tr = Translate
structure Frame : FRAME = MipsFrame
structure F = Frame
structure A = Assem

(* Get the allocated register for a given temp *)
fun allocatedReg alloc temp =
    case Temp.Table.look(alloc,temp) of
        SOME(r) => r
      | NONE => Frame.tempToString temp

(* Built on from insn *)
fun emitproc out (F.PROC{body,frame}) =
    let val _ = print ("emit " ^ Symbol.name(Frame.name(frame)) ^ "\n")
        val linearized = Canon.linearize body
        val canonized = Canon.traceSchedule(Canon.basicBlocks linearized)
	      val instrs = List.concat(map (MipsGen.codegen frame) canonized)
        val instrs2 = Frame.procEntryExit2 (frame,instrs)
        val (instrs2',alloc) = RegAlloc.alloc(instrs2,frame)
        val {prolog,body,epilog} = Frame.procEntryExit3(frame,instrs2')
        val format0 = Assem.format(allocatedReg alloc)
        (* val pr = fn i => TextIO.output(TextIO.stdOut, (format0 i))
        val _ = print prolog
        val _ = app pr body
        val _ = print epilog *)
    in
      TextIO.output(out,prolog);
      app (fn i => TextIO.output(out,(format0 i))) body;
      TextIO.output(out,epilog)
    end

  | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

fun withOpenFile fname f =
    let val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out)
    end

(* Compile and write to "[filename].s" *)
fun compile filename =
    let val absyn = Parse.parse filename
        (* val _ = PrintAbsyn.print(TextIO.stdOut, absyn) *)
        val frags = (FindEscape.findEscape absyn;
                     Semant.transProg absyn)
        (* val printTree = fn (frag) => case frag of F.PROC({body, frame}) => Printtree.printtree(TextIO.stdOut, body)
                                                | _ => ()
        val _ = map printTree frags *)
        val isProc = fn (frag) => case frag of F.PROC(_)   => true
                                             | F.STRING(_) => false
        val (procedures, strings) = List.partition isProc frags
    in
      withOpenFile (filename ^ ".s")
	                 (fn out =>
                       let in
                         TextIO.output(out, "\t.globl main\n");
                         TextIO.output(out, "\t.data\n"); (* Data section *)
                         app (emitproc out) strings; (* String Data *)
                         TextIO.output(out, "\n\t.text\n"); (* Text section *)
                         app (emitproc out) procedures (* Program Data *)
                       end)
    end

fun main (cmd: string, args: string list) =
    let in app compile args; 0 end
end
