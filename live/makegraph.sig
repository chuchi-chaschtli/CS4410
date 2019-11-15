signature MAKEGRAPH =
sig
  structure Flow : FLOW
  val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end
