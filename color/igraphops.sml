structure IGraphOps = struct

structure IT = Liveness.IGraph.Table

(* abstraction over entering elements into InterferenceGraph table *)
fun populateTable(tbl) = foldl (fn (v, t) => IT.enter(t, v, ())) IT.empty tbl

(* Set difference of two IGraphs by populating table of vertices in b, then
folding over elements in a to filter out elements *)
fun diff (a, b) =
  let
    val table = populateTable b
    fun helper (v, acc) =
      case IT.look(table, v)
        of SOME _ => acc
         | NONE => v::acc
  in
    foldl helper nil a
  end

(* Set union of two IGraphs by populating table of vertices in a, then folding
over vertices in b *)
fun union(a, b) =
  let
    val table = populateTable a
    fun helper (v, acc) =
      case IT.look(table, v)
        of SOME _ => v::acc
         | NONE => acc
  in
    foldl helper a b
  end

end
