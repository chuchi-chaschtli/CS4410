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

      fun buildTable func =
        foldl (fn (v, table) => IT.enter(table, v, func v)) IT.empty vertices

      val neighborsTable = buildTable IGraph.adj

      val degreeTable =
        let fun degree v = length (look neighborsTable v)
        in buildTable degree
        end

      fun neighbors (v, stack) = IGraphOps.diff(look neighborsTable v, stack)

      val simplifyWorklist = List.filter (fn v => look degreeTable v < nregs) notColored

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
      fun simplify (nil, _, stack) = stack
        | simplify (wl, degrees, stack) =
          let
            val (v, wlTail) = pop wl
            val stack' = push(stack, v)
            val neighboring = neighbors(v, stack')
            val (degrees', wl') = foldl (fn (v, (deg, stack)) => decrDegree(v, deg, stack))
                                        (degrees, wlTail)
                                        neighboring
          in
            simplify(wl', degrees', stack')
          end

      val selectStack = simplify(simplifyWorklist, degreeTable, nil)

      fun assignColors() =
        let
          (* Gets the colors of already colored neighbors to determine what
          colors are usable for the currently processed vertex *)
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
    in
      (assignColors(), nil)
    end
end
