structure Analysis : sig val check : string -> Tree.exp end =
struct
  fun check filename =
    let
      val absyn = Parse.parse filename
    in
      Semant.transProg absyn
    end
end
