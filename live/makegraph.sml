structure MakeGraph: MAKEGRAPH =

struct

structure Flow = Flow
structure A = Assem

structure LabelVertexGraph = BinaryMapFn(LabelOrdKey)

fun instrs2graph insns =
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
  end
end
