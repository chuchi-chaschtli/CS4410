fun labelCompare (a, b) = String.compare(Symbol.name a, Symbol.name b)

structure LabelOrdKey =
  struct
    type ord_key = Temp.label
    val compare = labelCompare
  end

structure TempOrdKey =
  struct
    type ord_key = Temp.temp
    val compare = Int.compare
  end
