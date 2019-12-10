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
      | getVertexWithLabel (label, (v, SOME l)::vertices) =
        if l = label
        then v
        else getVertexWithLabel(label, vertices)
      | getVertexWithLabel(label, v::vs) = getVertexWithLabel(label, vs)

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
  en

(* fun instrs2graph insns =
  let
    val control = Graph.newGraph()

    fun mkVertices(insn::insns, table, labelVertices, defs, uses, moves, labels, stack) =
        let
          val top = Graph.newNode(control)
          fun updateTable (tbl, values) =
            Graph.Table.enter(tbl, top, values)
          val newTable = updateTable(table, insn)
          fun populateLabelVertexGraph() =
            foldr (fn (vertex, label) => LabelVertexGraph.insert(label, vertex, top)) labelVertices labels
          val newLabelVertices = populateLabelVertexGraph()
        in
          (case insn
            of A.LABEL {assem, lab} =>
                mkVertices(insns, table, labelVertices, defs, uses, moves, lab::labels, stack)
             | A.MOVE {assem, dst, src} =>
                let
                  val newDefs = updateTable(defs, dst::nil)
                  val newUses = updateTable(uses, src::nil)
                  val newMoves = updateTable(moves, true)
                in mkVertices(insns, newTable, newLabelVertices, newDefs, newUses, newMoves, nil, top::stack)
                end
             | A.OPER {assem, src, dst, ... (* TODO Jump was unused otherwise *)} =>
               let
                 val newDefs = updateTable(defs, dst)
                 val newUses = updateTable(uses, src)
                 val newMoves = updateTable(moves, false)
               in mkVertices(insns, newTable, newLabelVertices, newDefs, newUses, newMoves, nil, top::stack)
               end)
        end
      | mkVertices(nil, table, labelVertices, defs, uses, moves, labels, stack) = (table, labelVertices, defs, uses, moves, stack)

    val empty = Graph.Table.empty
    val (table, labelVertices, def, use, ismove, stack) =
      mkVertices(insns, empty, LabelVertexGraph.empty, empty, empty, empty, nil, nil)

    fun mkEdges (curr::prev::prevs) =
        let
          fun jmps vertex =
            let
              val insn =
                case Graph.Table.look(table, vertex)
                 of SOME i => i
                  | NONE => ErrorMsg.impossible "Vertex could not be found in instructions table"

              val jmpLabels =
                case insn
                  of A.OPER {assem, src, dst, jump = SOME labels} => labels
                   | _ => nil

              fun vertices nil = nil
                | vertices (label::labels) =
                  case LabelVertexGraph.find(labelVertices, label)
                    of SOME vertex => vertex::vertices(labels)
                     | _           => vertices(labels) (* TODO ErrorMsg.impossible "Jump Label could not be found in label vertex graph" *)
            in
              vertices jmpLabels
            end

            val currJmps = jmps curr
            val prevJmps = jmps prev
        in
          (map (fn to => Graph.mk_edge {from=curr, to=to}) currJmps;
           map (fn to => Graph.mk_edge {from=prev, to=to}) prevJmps;
           case prevJmps
             of nil => Graph.mk_edge {from=prev, to=curr}
              | _ => ();
           mkEdges(prev::prevs))
        end
      | mkEdges (prev::nil) = ()
      | mkEdges nil = ()
  in
    (mkEdges stack;
     (Flow.FGRAPH {control=control, def=def, use=use, ismove=ismove}, Graph.nodes control))
  end *)
end
