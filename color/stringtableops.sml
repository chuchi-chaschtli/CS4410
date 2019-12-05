structure STOps = struct

structure StringOrdKey =
  struct
    type ord_key = string
    val compare = String.compare
  end

structure ST = BinaryMapFn(StringOrdKey)

(* abstraction over entering elements into string table *)
fun populateTable(tbl) = foldl (fn (v, t) => ST.insert(t, v, ())) ST.empty tbl

(* Set difference of two string sets by populating table of vertices in b, then
folding over elements in a to filter out elements *)
fun diff (a, b) =
  let
    val table = populateTable b
    fun helper (v, acc) =
      case ST.find(table, v)
        of SOME _ => acc
         | NONE => v::acc
  in
    foldl helper nil a
  end
end
