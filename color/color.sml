structure Color :> COLOR =
struct
  structure Frame : FRAME = MipsFrame

  type allocation = Frame.register Temp.Table.table

  datatype 'a LinkedList = Empty | Cons of 'a LinkedList * 'a * 'a LinkedList
  type NodeSet = Graph.node LinkedList
  type NodeWorklist = Graph.node List
  type NodeStack = Graph.node List (* TODO do we want a proper Stack? *)

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
end