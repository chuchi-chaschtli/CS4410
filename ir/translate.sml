structure F = Frame

signature TRANSLATE =
sig
    type exp
    type level
    type access (* Not same as Frame.access *)

    val outermost : level
    val newLevel : {parent:level, name:Temp.label, formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access
end

structure Translate : TRANSLATE =
struct
  type exp = unit
  datatype level = LEVEL of {frame: F.frame, parent: level}
                 | GLOBAL (* base-case being the top level *)
  type access = level * F.access

  val outermost = GLOBAL

  fun newLevel {parent=parent, name=name, formals=formals} =
    LEVEL{frame=F.newFrame({name=name, formals=true::formals}), parent=parent}

  fun formals level =
    (case level
      of LEVEL {frame, parent} => let
                                    val formals = F.formals frame
                                  in
                                    map (fn access => (level, access)) formals
                                  end
       | GLOBAL => nil)

  (* Alocate local access for a given level *)
  fun allocLocal level escape =
    (case level
      of LEVEL {frame, parent} => let val f_access = F.allocLocal frame escape
                                  in
                                    (level, f_access)
                                  end)
end
