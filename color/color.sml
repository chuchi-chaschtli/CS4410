(* structure Color :> COLOR =
struct
  structure Frame : FRAME = MipsFrame

  type allocation = Frame.register Temp.Table.table

  datatype 'a LinkedList = Empty | Cons of 'a LinkedList * 'a * 'a LinkedList
  type NodeSet = Graph.node LinkedList
  type NodeWorklist = Graph.node list
  type NodeStack = Graph.node list (* TODO do we want a proper Stack? *)

  (* NODE WORK-LISTS, SETS, AND STACKS *)
  val precolored : NodeSet = Empty (* machine registers, preassigned a color *)
  val initial    : NodeSet = Empty (* temporary registers, not precolored and not yet processed *)

  val simplifyWorklist : NodeWorklist = nil (* list of low-degree non-move-related nodes *)
  val freezeWorklist   : NodeWorklist = nil (* low-degree move-related nodes *)
  val spillWorklist    : NodeWorklist = nil (* high-degree nodes *)

  val spilledNodes : NodeSet = Empty (* nodes marked for spilling during this round; initially empty *)
  val coalescedNodes : NodeSet = Empty (* registers that have been coalesced; when u ← v is coalesced, v is added to this set and u put back on some work-list (or vice versa) *)
  val coloredNodes : NodeSet = Empty (* nodes successfully colored *)

  val selectStack : NodeStack = nil (* stack containing temporaries removed from the graph. *)

  (* MOVE SETS *)
  (* TODO Level 2 so I don't think we'll need any of this initially *)
  val coalescedMoves = () (* moves that have been coalesced *)
  val constrainedMoves = () (* moves whose source and target interfere *)
  val frozenMoves = () (* moves that will no longer be considered for coalescing *)
  val worklistMoves = () (* moves enabled for possible coalescing *)
  val activeMoves = () (* moves not yet ready for coalescing. *)

  (* ADDITIONAL DATA STRUCTURES *)
  val adjSet = () (* the set of interference edges (u, v) in the graph. If (u, v) ∈ adjSet then (v, u) ∈ adjSet *)
  val adjList = () (* adjacency list representation of the graph; for each non-precolored temporary u, adjList[u] is the set of nodes that interfere with u *)
  val degree = () (* an array containing the current degree of each node *)
  val moveList = () (* a mapping from a node to the list of moves it is associated with *)
  val alias = () (* when a move (u, v) has been coalesced, and v put in coalescedNodes, then alias(v) = u *)
  val nodeColor = () (* the color chosen by the algorithm for a node; for precolored nodes *)

  (* TODO *)
  fun color({interference, initial, spillCost, registers}) = ()
end *)

structure Color : COLOR =
struct
  structure Frame = MipsFrame
  structure IGraph = Liveness.IGraph
  structure IT = IGraph.Table
  type allocation = Frame.register Temp.Table.table

  val nregs = 30 (* TODO: don't hardcode this? *)

  fun peek nil = nil
    | peek(node::nodes) = node
  fun push (nodes, node) = node::nodes
  fun pop nil = ErrorMsg.impossible "Worklist is empty"
    | pop (node::nodes) = (node, nodes)

  fun color {interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, registers} =
    let
      look t k =
        case IT.look(t, k)
          of SOME a => a
           | NONE _ => ErrorMsg.impossible "table does not contain given key"
      val vertices = IGraph.nodes graph
      fun addColoredReg(r, acc) =
          let val reg = tnode(r)
          in reg::acc
          end
          handle TempNotFound => acc
      val alreadyColored = foldl (fn (r, vs) => addColoredReg(r, vs)) nil Frame.registerTemps
      val notColored = IGraphOps.diff(vertices, alreadyColored)
      val neighborsTable = foldl (fn (v, table) => IT.enter(table, v, IGraph.adj v))
                                 IT.empty
                                 vertices

      fun degree v = length (look neighborsTable v)
      val degreeTable = foldl (fn (v, table) => IT.enter(table, v, degree v))
                              IT.empty
                              vertices

      fun neighbors (v, stack) = IGraphOps.diff(look neighborsTable v, stack)
      val simplifyWorklist =
        let
          fun process(wl, nil) = wl
            | process(wl, vertex::vertices) =
              if look degreeTable vertex < nregs
              then process(vertex::wl, vertices)
              else process(wl, vertices)
        in
          process(nil, uncolored)
        end
      fun decrDegree (v, degrees, wl) =
        let
          val deg = (look degrees v) - 1
          val degrees' = IT.enter(degrees, v, deg)
          val wl' = if deg = nregs - 1
                    then IGraphOps.union(wl, v::nil)
                    else wl
        in
          (degrees', wl')
        end
      fun simplify (wl, degrees, stack) =
        let
          val (v, wlTail) = pop wl
          val stack' = push(stack, v)
          val neighboring = neighbors(v, stack')
          val (degrees', wl') = foldl (fn (v, (deg, stack)) => decrDegree(v, deg, stack))
                                      (degrees, wlTail)
                                      neighboring
        in
          (wl', degrees', stack')
        end

      (* TODO * transofrm this into a fold, I couldn't figure it out *)
      fun processWl (nil, _, stack) = stack
        | processWl (wl, degrees, stack) =
          let
            val (wl', degrees', stack') = simplify(wl, degrees, stack)
          in
            processWl(wl', degrees', stack')
          end
      selectStack = processWl(nil, simplifyWorklist, degreeTable)

      fun assignColors() =
        let
          fun getColors(nil, color) = nil
            | getColors(neighbor::rest, color) =
              case Temp.table.look(color, (gtemp neighbor))
                of SOME x => x::getColors(rest, color)
                 | NONE _ => getColors(rest, color)
          and process(color, nil) = color
            | process(color, vertex::rest) =
              let
                val neighbors = lookup neighborsTable (gtemp vertex)
                val usedColors = getColors(neighbors, color)
                val okColors = IGraphOps.difference(registers, usedColors)
                val nextColor = Temp.table.enter (color, gtemp vertex, (hd okColors))
                                handle Empty => () (* TODO This is where we would spill *)
              in
                process(rest, nextColor)
              end
        in
          process(initial, selectStack)
        end

	    val coloring = assignColors()

      (* TODO * check invariants (pg 244) *)
    in
    end
end
