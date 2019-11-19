structure Liveness : LIVENESS =
struct

structure F = Flow
structure G = F.Graph
structure T = G.Table
structure IGraph = Graph

structure TempListSet = ListSetFn(TempOrdKey)

datatype igraph =
  IGRAPH of {graph: IGraph.graph,
             tnode: Temp.temp -> IGraph.node,
             gtemp: IGraph.node -> Temp.temp,
             moves: (IGraph.node * Graph.node) list}

type liveVarSet = unit Temp.Table.table * Temp.temp list
type liveVarMap = liveVarSet T.table

fun mkLiveSets(vertices) =
  foldl (fn (vertex, table) => T.enter(table, vertex, TempListSet.empty)) T.empty vertices

fun show (outstream, IGRAPH{graph, tnode, gtemp, moves}) = ()

fun interferenceGraph (F.FGRAPH{control, def, use, ismove}) =
  let
    val vertices = rev (G.nodes control)
    val listSetFunctor = fn list => TempListSet.addList(TempListSet.empty, list)
    val liveIn = mkLiveSets(vertices)
    val liveOut = mkLiveSets(vertices)

    val isModified = false
    fun processVertices (nil, liveIns, liveOuts) =
        (liveIns, liveOuts)
      | processVertices (vertex::vertices, liveIns, liveOuts) =
        let
          val uses = listSetFunctor(case T.look(use, vertex) of SOME x => x | _ => nil)
          val defs = listSetFunctor(case T.look(def, vertex) of SOME x => x | _ => nil)
          val liveIn' = case T.look(liveIns, vertex) of SOME x => x | _ => TempListSet.empty
          val liveOut' = case T.look(liveOuts, vertex) of SOME x => x | _ => TempListSet.empty

          val liveIn'' = TempListSet.union(uses, TempListSet.difference(liveOut', defs))
          val liveOut'' =
            foldl (fn (vs, liveInSet) =>
                    TempListSet.union(case T.look(liveIns, vs) of SOME x => x | _ => TempListSet.empty, liveInSet))
                  TempListSet.empty
                  (G.succ(vertex))
        in
          (isModified = isModified orelse
                        TempListSet.equal(liveOut', liveOut'') andalso not(TempListSet.equal(liveIn', liveIn''));
           processVertices(vertices, T.enter(liveIns, vertex, liveIn''), T.enter(liveOuts, vertex, liveOut'')))
        end

    fun liveness (liveIns, liveOuts) =
      let
        val (liveIns', liveOuts') = processVertices(vertices, liveIns, liveOuts)
      in
        if not isModified
        then liveOuts'
        else liveness(liveIns', liveOuts')
      end

    val liveMap = liveness(liveIn, liveOut)
    val g = IGraph.newGraph()

    (* TODO : make vertices/edges in interference graph *)
  in
    ()
  end

end
