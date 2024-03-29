structure Main = struct
  fun printInsnList(insnList, coloring) =
    let
      fun getsome (SOME x) = x
      val getRegister = fn temp => getsome(Temp.Table.look(coloring, temp))
      val format0 = Assem.format(getRegister)
      val printOut = fn i => print (format0 i)
    in
      app printOut insnList
    end

  fun printAllocated (Frame.PROC{body, frame}) =
    let val stms = Canon.linearize body
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
        val instrs = List.concat(map (MipsGen.codegen frame) stms')
        val (allocated, coloring) = RegAlloc.alloc(instrs, frame)
    in
      printInsnList(allocated, coloring)
    end
    | printAllocated (F.STRING(lab,s)) = ()

  fun compile filename =
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
    in
      app printAllocated frags
    end
end
