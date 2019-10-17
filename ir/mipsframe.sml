structure MipsFrame : FRAME =
struct
datatype access = InFrame of int | InReg of Temp.temp
type frame = {name: Temp.label, frameOffset: int ref, formals: access list}
val wordLenBytes = 4

fun newFrame {name, formals} =
    let
    	val paramIndex = ref 1
    	fun getParamOffset =
	      let val tmp = !paramIndex
	      in paramIndex := tmp+1; tmp * wordLenBytes
	      end
	    val formals' = map (fn escape => if escape
                  					           then InFrame (getParamOffset())
            					                 else InReg   (Temp.newtemp())) formals
    in
	     {name=name, frameOffset=(ref 0), formals=formals'}
    end

fun allocLocal (frame:frame) escape =
    let
    	val {name=name, frameOffset=offset, formals=formals} = frame (* TODO: Do we need name/formals? *)
    	val updatedOffset = !frameOffset - wordLenBytes
    in
    	offset := newFrameOffset;
    	if escape
    	then InReg   (Temp.newtemp())
    	else InFrame (updatedOffset)
    end

fun name (frame:frame) = (#name frame)
fun formals (frame:frame) = (#formals frame)
end
