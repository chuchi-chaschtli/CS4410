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

type liveSet = unit Temp.Table.table * Temp.temp list
type liveMap = liveSet T.table

fun mkLiveSets(vertices) =
  foldl (fn (vertex, table) => T.enter(table, vertex, TempListSet.empty)) T.empty vertices

fun show (outstream, IGRAPH{graph, tnode, gtemp, moves}) = ()

fun interferenceGraph (F.FGRAPH{control, def, use, ismove}) =
  let
    val vertices = rev (G.nodes control)
    val listSetFunctor = fn list => TempListSet.addList(TempListSet.empty, list)
    val listFindFunctor = fn (l, v, default) => case T.look(l, v) of SOME element => element | _ => default
    val liveIn = mkLiveSets(vertices)
    val liveOut = mkLiveSets(vertices)

    val isModified = false
    fun processVertices (nil, liveIns, liveOuts) =
        (liveIns, liveOuts)
      | processVertices (vertex::vertices, liveIns, liveOuts) =
        let
          val uses = listSetFunctor(listFindFunctor(use, vertex, nil))
          val defs = listSetFunctor(listFindFunctor(def, vertex, nil))
          val liveIn' = listFindFunctor(liveIns, vertex, TempListSet.empty)
          val liveOut' = listFindFunctor(liveOuts, vertex, TempListSet.empty)

          val liveIn'' = TempListSet.union(uses, TempListSet.difference(liveOut', defs))
          val liveOut'' =
            foldl (fn (vs, liveInSet) =>
                    TempListSet.union(listFindFunctor(liveIns, vs, TempListSet.empty), liveInSet))
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

    fun processIGraphVertices() =
      let
        fun mkVertices (v, (vertexGraph, tempGraph)) =
          let
            fun mkVertex(t, (vertexGraph, tempGraph)) =
              case Temp.Table.look(tempGraph, t)
                of NONE =>
                   let val vertex = IGraph.newNode(g)
                   in (T.enter(vertexGraph, vertex, t), Temp.Table.enter(tempGraph, t, vertex))
                   end
                 | _ => (vertexGraph, tempGraph)
            val (vertexGraph', tempGraph') = foldl mkVertex
                                                   (vertexGraph, tempGraph)
                                                   (listFindFunctor(def, v, nil))
          in
            foldl mkVertex (vertexGraph', tempGraph') (listFindFunctor(use, v, nil))
          end
      in
        foldl mkVertices (T.empty, Temp.Table.empty) vertices
      end

    val (vertexGraph, tempGraph) = processIGraphVertices()

    val moves = ref nil
    fun inMoves(a, b) =
      let
        (* Check both sides to avoid adding duplicates *)
        fun equal(a, b, m, n) = IGraph.eq(a, m) andalso IGraph.eq(b, n) orelse IGraph.eq(a, n) andalso IGraph.eq(b, m)
        fun exists(a, b) = List.exists(fn (m, n) => equal(a, b, m, n)) (!moves)
      in
        exists(a, b)
      end

    fun processIGraphEdges() =
      let
        fun mkEdges (v) =
          let
            val isMove = listFindFunctor(ismove, v, false)
            val isUseMove = fn tmp => TempListSet.member(listSetFunctor(listFindFunctor(use, v, nil)), tmp)
            fun areNeighbors(v, u) =
              let
                fun exists(v, u) = List.exists(fn w => IGraph.eq(w, v)) (IGraph.adj u)
              in
                exists(v, u) orelse exists(u, v)
              end
            fun mkEdge (a, b) =
              let
                val v = vertex(a)
                val u = vertex(b)
                val nothingToDo = a = b orelse areNeighbors(v, u)
                fun mkMove() =
                  if isUseMove b andalso isMove andalso not(inMoves(v, u))
                  then moves := (v, u)::(!moves)
                  else ()
              in
                if nothingToDo then ()
                else if isUseMove b andalso isMove then mkMove()
                else IGraph.mk_edge{from=u, to=v}
              end
            fun build(nil, lives) = ()
              | build(def::defs, lives) =
                let
                  fun buildForDef nil = ()
                    | buildForDef(live::lives) = (mkEdge(def, live); buildForDef(lives))
                in
                  (buildForDef(TempListSet.listItems lives); build(defs, lives))
                end
          in
            build(listFindFunctor(def, v, nil), listFindFunctor(liveMap, v, TempListSet.empty))
          end
      in
        map mkEdges vertices
      end
    and tmp vertex =
      case T.look(vertexGraph, vertex)
        of SOME t => t
         | _ => ErrorMsg.impossible "Vertex not found in temps"
    and vertex tmp =
      case Temp.Table.look(tempGraph, tmp)
        of SOME v => v
         | _ => ErrorMsg.impossible "Temp not found in vertices"


    val _ = processIGraphEdges()
  in
    (IGRAPH {graph=g, tnode=vertex, gtemp=tmp, moves=(!moves)},
     fn vertex => TempListSet.listItems (listFindFunctor(liveMap, vertex, TempListSet.empty)))
  end
end
