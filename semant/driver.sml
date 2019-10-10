structure TypeCheck : sig val check : string -> Types.ty end =
struct
  fun check filename =
    let
      val absyn = Parse.parse filename
      val {exp = translated, ty = checkedType} = Semant.transProg absyn
    in
      checkedType
    end
end
