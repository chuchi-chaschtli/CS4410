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

  fun compile filename =
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        val allocated = map getinstrs frags
    in
      map (fn insnList => map (fn insn => print(#assem insn)) insnList) allocated
      
    end
end
