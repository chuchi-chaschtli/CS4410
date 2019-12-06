structure Main = struct
  fun getsome (SOME x) = x

  fun getinstrs (Frame.PROC{body, frame}) =
    let val stms = Canon.linearize body
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
        val instrs = List.concat(map (MipsGen.codegen frame) stms')
        val (allocated, coloring) = RegAlloc.alloc(instrs, frame)
    in
      allocated
    end
    | getinstrs (F.STRING(lab,s)) = nil

  fun printInsnList insnList =
    let
      val format0 = Assem.format(Temp.makestring)
      val printOut = fn i => print (format0 i)
    in
      app printOut insnList
    end
    

  fun compile filename =
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        val allocated = map getinstrs frags
    in
      app (fn insnList => printInsnList insnList) allocated
    end
end
