signature LIVENESS =
sig
  structure IGraph : GRAPH
  datatype igraph =
    IGRAPH of {graph: IGraph.graph,
               tnode: Temp.temp -> IGraph.node,
               gtemp: IGraph.node -> Temp.temp,
               moves: (IGraph.node * Graph.node) list}
  val interferenceGraph : Flow.flowgraph -> unit (* TODO: igraph * (Flow.Graph.node -> Temp.temp list) *)

  val show: TextIO.outstream * igraph -> unit

end
