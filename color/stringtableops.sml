structure STOps = struct

structure StringOrdKey =
  struct
    type ord_key = string
    val compare = String.compare
  end

structure ST = BinarySetFn(StringOrdKey)
end
