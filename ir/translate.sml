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
datatype level = LEVEL of {frame: F, parent: level}
               | GLOBAL (* base-case being the top level *)
type access = level * F.access

val outermost = GLOBAL

fun newLevel {parent=parent, name=name, formals=formals} =
  LEVEL {frame=F.newFrame(name, [true]@formals), parent=parent}

fun formals level =
  (case level
     of LEVEL {frame, _} = map (fn formal => (level, formal)) F.formals(frame)
      | GLOBAL = nil)

fun allocLocal lvl escape =
  let val access=F.allocLocal (#frame lvl) escape
  in
	  (lvl, access)
  end
end
