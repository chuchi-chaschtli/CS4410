structure MakeGraph: MAKEGRAPH =

struct

structure Flow = Flow
structure A = Assem

fun instrs2graph insns =
  let
    val control = Graph.newGraph()
    (* Label-vertex connections for graph construction *)
    fun mkLabelVertex insn =
      case insn
        of A.LABEL {assem, lab} => (Graph.newNode control, SOME lab)
         | _ => (Graph.newNode control, NONE)
    val labelVertices = map mkLabelVertex insns
    val vertices = map (fn (v, _) => v) labelVertices

    fun getVertexWithLabel (label, nil) = ErrorMsg.impossible "Label not found"
      | getVertexWithLabel (label, (v, NONE)::vertices) = getVertexWithLabel(label, vertices)
      | getVertexWithLabel (label, (v, SOME l)::vertices) =
        if l = label
        then v
        else getVertexWithLabel(label, vertices)

    (*
    For each jmp, find corresponding vertex that has label -> construct edge
    For each non-jmp, just make an edge to next insn
    *)
    fun mkJumpEdge(vertex, insn) =
      case insn
        of A.OPER {assem, dst, src, jump} =>
          (case jump
            of SOME(jumps) =>
              app (fn l => Graph.mk_edge {from = vertex, to = getVertexWithLabel(l, labelVertices)}) jumps
             | _ => ())
         | _ => ()

    fun mkEdges nil = ()
      | mkEdges (prev::nil) = mkJumpEdge prev
      | mkEdges ((currV, currI)::(prevV, prevI)::vertexInsns) =
        (mkJumpEdge (currV, currI);
         Graph.mk_edge {from=currV, to=prevV};
         mkEdges((prevV, prevI)::vertexInsns))

    (* Construct the graph by folding over each vertex/insn pair with the three
    data structures we need - defs, uses, ismoves *)
    fun mkGraph((vertex, insn), (def, use, ismove)) =
      case insn
        of A.OPER {assem, dst, src, ...} => (
            Graph.Table.enter(def, vertex, dst),
            Graph.Table.enter(use, vertex, src),
            Graph.Table.enter(ismove, vertex, false)
          )
         | A.MOVE {assem, dst, src} => (
            Graph.Table.enter(def, vertex, dst::nil),
            Graph.Table.enter(use, vertex, src::nil),
            Graph.Table.enter(ismove, vertex, true)
          )
         | _ => (
             Graph.Table.enter(def, vertex, nil),
             Graph.Table.enter(use, vertex, nil),
             Graph.Table.enter(ismove, vertex, false)
           )

    val vertexInsns = ListPair.map (fn (v, i) => (v, i)) (vertices, insns)
    val _ = mkEdges (vertexInsns)
    val (def, use, ismove) = foldl (fn e => mkGraph e) (Graph.Table.empty, Graph.Table.empty, Graph.Table.empty) vertexInsns
  in
    (Flow.FGRAPH{control=control,def=def,use=use,ismove=ismove}, vertices)
  end
end
