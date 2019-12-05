structure RegAlloc : REG_ALLOC =
struct
  structure Frame = MipsFrame
  structure IGraph = Graph
  type allocation = Frame.register Temp.Table.table

  fun alloc (instrs, frame) =
    let
      val (flowgraph, _) = MakeGraph.instrs2graph instrs
      val (interference, _) = Liveness.interferenceGraph flowgraph

      val (returnAllocation, _) =
        Color.color {interference = interference,
                     initial = Frame.tempMap,
                     spillCost = (fn n => 0) (* TODO: Unused currently *),
                     registers = Frame.registerTempNames}
    in
      (instrs, returnAllocation)
    end
end
