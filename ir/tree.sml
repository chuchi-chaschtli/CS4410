signature TREE =
sig
    type label = Temp.label
    type size

    datatype stm = SEQ of stm * stm
                 | LABEL of label
                 | JUMP of exp * label list
                 | CJUMP of relop * exp * exp * label * label
                 | MOVE of exp * exp
                 | EXP of exp

    and exp = BINOP of binop * exp * exp
            | MEM of exp
            | TEMP of Temp.temp
            | ESEQ of stm * exp
            | NAME of label
            | CONST of int
            | CALL of exp * exp list

    and binop = PLUS | MINUS | MUL | DIV | AND
              | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

    and relop = EQ | NE | LT | GT | LE | GE
              | ULT | ULE | UGT | UGE

    val notRel : relop -> relop
    val commute: relop -> relop
end

structure Tree : TREE =
struct
    type label=Temp.label
    type size = int

    datatype stm = SEQ of stm * stm
                 | LABEL of label
                 | JUMP of exp * label list
                 | CJUMP of relop * exp * exp * label * label
                 | MOVE of exp * exp
                 | EXP of exp

    and exp = BINOP of binop * exp * exp
            | MEM of exp
            | TEMP of Temp.temp
            | ESEQ of stm * exp
            | NAME of label
            | CONST of int
            | CALL of exp * exp list

    and binop = PLUS | MINUS | MUL | DIV | AND
        | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

    and relop = EQ | NE | LT | GT | LE | GE
              | ULT | ULE | UGT | UGE

    val notRel = fn relop => EQ (* TODO - implement ... I couldn't find any information in the book, maybe it's just the opposite of each case? EQ = NE, etc?*)

    fun commute EQ  = EQ
      | commute NE  = NE
      | commute LE  = GE
      | commute GT  = LT
      | commute GE  = LE
      | commute LT  = GT
      | commute ULE = UGE
      | commute UGT = ULT
      | commute UGE = ULE
      | commute ULT = UGT
end
