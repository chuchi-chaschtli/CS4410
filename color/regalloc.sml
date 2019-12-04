structure RegAlloc :> REG_ALLOC =
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
                     spillCost = (fn n => -1) (* Unused currently *),
                     registers = Frame.tempMapRegisterNames (* TODO get the names of each register, like $fp, $zero, $s0, $s1 *)}
    in
      (instrs, returnAllocation)
    end
end
