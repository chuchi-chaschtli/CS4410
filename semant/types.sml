structure Types =
struct

  type unique = unit ref

  datatype ty =
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
          | NAME of Symbol.symbol * ty option ref
          | UNIT

  fun toString (ty.RECORD _) = "RECORD"
    | toString ty.NIL = "NIL"
    | toString ty.INT = "INT"
    | toString ty.STRING = "STRING"
    | toString (ty.ARRAY _) = "ARRAY"
    | toString (ty.NAME (name, _)) = ("NAME = " ^ (Symbol.name name))
    | toString ty.UNIT = "UNIT"

end
