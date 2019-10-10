structure TypeCheck : sig val check : string -> unit end =
struct
  fun check filename =
    let
      val absyn = Parse.parse filename
      val checked = Semant.transProg absyn
    in
      checked
    end
end
