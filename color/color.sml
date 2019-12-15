structure Color : COLOR =
struct
  structure Frame = MipsFrame
  structure TT = Temp.Table
  type allocation = Frame.register TT.table

  structure ST = STOps.ST

  val nregs = length Frame.registerTemps

  (* stack operations *)
  fun peek nil = nil
    | peek(node::nodes) = node
  fun push (nodes, node) = node::nodes
  fun pop nil = ErrorMsg.impossible "Worklist is empty"
    | pop (node::nodes) = (node, nodes)

  fun color {interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, registers} =
    let
      val colors = ST.addList(ST.empty, Frame.registerTempNames)

      fun findRemovableVertex nil = ErrorMsg.impossible "cannot pick removable vertex"
        | findRemovableVertex (vertex::vertices) =
          if length(Graph.adj vertex) < nregs
          then vertex
          else findRemovableVertex vertices

      fun rmVertexFrom(v, nil) = nil
        | rmVertexFrom(v, vertex::vertices) =
          if Graph.eq(v, vertex)
          then vertices
          else vertex::rmVertexFrom(v, vertices)

      fun build(stack, nil) = stack
        | build(stack, vertices) =
          let
            val v = findRemovableVertex vertices
            val vertices' = rmVertexFrom(v, vertices)
            val stack' = push(stack, v)
          in
            build(stack', vertices')
          end

      val stack = build(nil, Graph.nodes graph)

      (* Gets the colors of already colored neighbors to determine what
      colors are usable for the currently processed vertex *)
      fun getNeighborColors(color, nil, set) = set
        | getNeighborColors(color, neighbor::neighbors, set) =
          case TT.look(color, gtemp neighbor)
            of SOME x => getNeighborColors(color, neighbors, ST.add(set, x))
             | NONE   => getNeighborColors(color, neighbors, set)

      fun assignColors() =
        let
          fun getNextRegister(vertex, color) =
            let
              val neighbors = Graph.adj vertex
              val usedColors = getNeighborColors (color, neighbors, ST.empty)
              val okColors = ST.difference(colors, usedColors)
              val nextColor = hd (ST.listItems okColors)
                              handle Empty => ErrorMsg.impossible "Spill!!!"
            in
              nextColor
            end

          fun process (color, nil) = color
            | process (color, stack) =
              let
                val (v, stack') = pop stack
                val v' = gtemp v
              in
                case TT.look(color, v')
                  of SOME _ => process(color, stack')
                   | _ =>
                    let
                      val reg = getNextRegister(v, color)
                      val nextColor = TT.enter(color, v', reg)
                    in
                      process(nextColor, stack')
                    end
              end
        in
          process(initial, stack)
        end
    in
      (assignColors(), nil)
    end
end
