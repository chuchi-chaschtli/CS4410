structure Color : COLOR =
struct
  structure Frame = MipsFrame
  structure IGraph = Liveness.IGraph
  structure IT = IGraph.Table
  structure TT = Temp.Table
  type allocation = Frame.register TT.table

  val nregs = length Frame.registerTemps

  (* stack operations *)
  fun peek nil = nil
    | peek(node::nodes) = node
  fun push (nodes, node) = node::nodes
  fun pop nil = ErrorMsg.impossible "Worklist is empty"
    | pop (node::nodes) = (node, nodes)

  fun color {interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, registers} =
    let
      (* Abstraction of Table.look to print error if key not found *)
      fun look t k =
        case IT.look(t, k)
          of SOME v => v
           | NONE   => ErrorMsg.impossible "table does not contain given key"
      val vertices = IGraph.nodes graph

      (* Helper to add registers that are precolored using tnode *)
      fun addColoredReg(r, acc) =
          let val reg = tnode(r)
          in reg::acc
          end
          handle ErrorMsg.Error => acc

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

      (* Builds the simplifyWorklist by looking at degree of each vertex in the
      notColored set *)
      val simplifyWorklist =
        let
          fun process(wl, nil) = wl
            | process(wl, vertex::vertices) =
              if look degreeTable vertex < nregs
              then process(vertex::wl, vertices)
              else process(wl, vertices)
        in
          process(nil, notColored)
        end

      (* Helper to reduce degree of nodes during simplify step *)
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

      (* pops the simplifyWL and pushs that element onto the accumulating
      selectStack, then decrements degree of all neighbors *)
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

      val selectStack = processWl(nil, degreeTable, simplifyWorklist)

      fun assignColors() =
        let
          (* Gets the colors of already colored neighbors *)
          fun getColors(nil, color) = nil
            | getColors(neighbor::rest, color) =
              case TT.look(color, gtemp neighbor)
                of SOME x => x::getColors(rest, color)
                 | NONE   => getColors(rest, color)
          and process(color, nil) = color
            | process(color, vertex::rest) =
              let
                val neighbors = look neighborsTable vertex
                val usedColors = getColors(neighbors, color)
                val okColors = STOps.diff(registers, usedColors)
                val nextColor = TT.enter (color, gtemp vertex, hd okColors)
                                handle Empty => ErrorMsg.impossible "Spill!!!!" (* TODO This is where we would spill *)
              in
                process(nextColor, rest)
              end
        in
          process(initial, selectStack)
        end

	    val coloring = assignColors()
    in
      (coloring, nil)
    end
end
