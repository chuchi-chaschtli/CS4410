structure Main = struct
  fun getinstrs (Frame.PROC{body,frame}) =
    let val stms = Canon.linearize body
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
        val instrs = List.concat(map (MipsGen.codegen frame) stms')
    in
      instrs
    end
    | getinstrs (F.STRING(lab,s)) = nil

  fun compile filename =
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        val instrs = map getinstrs frags
        val flowgraphs = map (fn insrList => let val (graph, _) = MakeGraph.instrs2graph insrList in graph end) instrs
        val igraphs = map (fn graph => let val (graph, _) = Liveness.interferenceGraph graph in graph end) flowgraphs
    in
      app (fn graph => Liveness.show(TextIO.stdOut, graph)) igraphs
    end
end
