functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\161\000\005\000\161\000\007\000\161\000\009\000\161\000\
\\011\000\161\000\013\000\161\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\026\000\161\000\027\000\161\000\
\\031\000\161\000\032\000\161\000\035\000\161\000\036\000\161\000\
\\038\000\161\000\039\000\161\000\043\000\161\000\044\000\161\000\
\\045\000\161\000\000\000\
\\001\000\001\000\162\000\005\000\162\000\007\000\162\000\009\000\162\000\
\\011\000\162\000\013\000\162\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\026\000\162\000\027\000\162\000\
\\031\000\162\000\032\000\162\000\035\000\162\000\036\000\162\000\
\\038\000\162\000\039\000\162\000\043\000\162\000\044\000\162\000\
\\045\000\162\000\000\000\
\\001\000\001\000\163\000\005\000\163\000\007\000\163\000\009\000\163\000\
\\011\000\163\000\013\000\163\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\026\000\163\000\027\000\163\000\
\\031\000\163\000\032\000\163\000\035\000\163\000\036\000\163\000\
\\038\000\163\000\039\000\163\000\043\000\163\000\044\000\163\000\
\\045\000\163\000\000\000\
\\001\000\001\000\164\000\005\000\164\000\007\000\164\000\009\000\164\000\
\\011\000\164\000\013\000\164\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\026\000\164\000\027\000\164\000\
\\031\000\164\000\032\000\164\000\035\000\164\000\036\000\164\000\
\\038\000\164\000\039\000\164\000\043\000\164\000\044\000\164\000\
\\045\000\164\000\000\000\
\\001\000\001\000\165\000\005\000\165\000\007\000\165\000\009\000\165\000\
\\011\000\165\000\013\000\165\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\026\000\165\000\027\000\165\000\
\\031\000\165\000\032\000\165\000\035\000\165\000\036\000\165\000\
\\038\000\165\000\039\000\165\000\043\000\165\000\044\000\165\000\
\\045\000\165\000\000\000\
\\001\000\001\000\166\000\005\000\166\000\007\000\166\000\009\000\166\000\
\\011\000\166\000\013\000\166\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\026\000\166\000\027\000\166\000\
\\031\000\166\000\032\000\166\000\035\000\166\000\036\000\166\000\
\\038\000\166\000\039\000\166\000\043\000\166\000\044\000\166\000\
\\045\000\166\000\000\000\
\\001\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\030\000\011\000\033\000\010\000\034\000\009\000\
\\037\000\008\000\041\000\007\000\042\000\006\000\000\000\
\\001\000\002\000\042\000\000\000\
\\001\000\002\000\052\000\000\000\
\\001\000\002\000\070\000\000\000\
\\001\000\002\000\071\000\000\000\
\\001\000\002\000\072\000\000\000\
\\001\000\002\000\080\000\000\000\
\\001\000\002\000\104\000\012\000\103\000\029\000\102\000\000\000\
\\001\000\002\000\106\000\000\000\
\\001\000\002\000\109\000\000\000\
\\001\000\002\000\125\000\000\000\
\\001\000\002\000\131\000\000\000\
\\001\000\002\000\134\000\000\000\
\\001\000\005\000\095\000\013\000\094\000\000\000\
\\001\000\005\000\099\000\009\000\098\000\000\000\
\\001\000\005\000\120\000\009\000\119\000\000\000\
\\001\000\005\000\120\000\013\000\126\000\000\000\
\\001\000\006\000\088\000\028\000\087\000\000\000\
\\001\000\006\000\121\000\000\000\
\\001\000\006\000\129\000\019\000\128\000\000\000\
\\001\000\007\000\077\000\009\000\076\000\000\000\
\\001\000\007\000\077\000\039\000\100\000\000\000\
\\001\000\008\000\089\000\000\000\
\\001\000\011\000\084\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\026\000\021\000\
\\027\000\020\000\000\000\
\\001\000\011\000\097\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\026\000\021\000\
\\027\000\020\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\
\\031\000\075\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\
\\035\000\110\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\
\\036\000\074\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\
\\036\000\132\000\000\000\
\\001\000\019\000\086\000\000\000\
\\001\000\019\000\096\000\000\000\
\\001\000\019\000\136\000\000\000\
\\001\000\028\000\073\000\000\000\
\\001\000\028\000\118\000\000\000\
\\001\000\038\000\069\000\043\000\041\000\044\000\040\000\045\000\039\000\000\000\
\\001\000\040\000\116\000\000\000\
\\001\000\043\000\041\000\044\000\040\000\045\000\039\000\000\000\
\\139\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\010\000\019\000\014\000\018\000\028\000\017\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\151\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\
\\032\000\111\000\000\000\
\\152\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\153\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\154\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\017\000\029\000\018\000\028\000\000\000\
\\158\000\017\000\029\000\018\000\028\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\167\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\000\000\
\\168\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\000\000\
\\169\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\170\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\171\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\030\000\011\000\033\000\010\000\034\000\009\000\
\\037\000\008\000\041\000\007\000\042\000\006\000\000\000\
\\172\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\173\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\174\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\030\000\011\000\033\000\010\000\034\000\009\000\
\\037\000\008\000\041\000\007\000\042\000\006\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\002\000\080\000\000\000\
\\178\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\179\000\008\000\050\000\010\000\049\000\012\000\048\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\040\000\114\000\000\000\
\\183\000\045\000\039\000\000\000\
\\184\000\000\000\
\\185\000\043\000\041\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\002\000\109\000\000\000\
\\197\000\000\000\
\\198\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\199\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\\203\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\026\000\021\000\027\000\020\000\000\000\
\"
val actionRowNumbers =
"\007\000\048\000\050\000\044\000\
\\047\000\060\000\043\000\008\000\
\\007\000\007\000\007\000\070\000\
\\045\000\046\000\078\000\007\000\
\\009\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\100\000\084\000\
\\083\000\088\000\082\000\086\000\
\\041\000\010\000\011\000\012\000\
\\039\000\034\000\032\000\049\000\
\\027\000\069\000\076\000\007\000\
\\073\000\051\000\079\000\030\000\
\\067\000\066\000\005\000\004\000\
\\006\000\003\000\002\000\001\000\
\\065\000\064\000\063\000\062\000\
\\099\000\087\000\085\000\070\000\
\\036\000\024\000\029\000\007\000\
\\007\000\007\000\053\000\007\000\
\\075\000\020\000\037\000\031\000\
\\021\000\072\000\080\000\028\000\
\\014\000\007\000\015\000\095\000\
\\033\000\058\000\056\000\068\000\
\\054\000\013\000\007\000\081\000\
\\052\000\007\000\061\000\089\000\
\\042\000\095\000\090\000\097\000\
\\040\000\093\000\022\000\025\000\
\\007\000\007\000\074\000\077\000\
\\007\000\071\000\017\000\023\000\
\\007\000\026\000\016\000\018\000\
\\035\000\057\000\055\000\092\000\
\\091\000\098\000\007\000\019\000\
\\094\000\096\000\007\000\101\000\
\\038\000\059\000\007\000\102\000\
\\000\000"
val gotoT =
"\
\\001\000\136\000\002\000\003\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\036\000\010\000\035\000\011\000\034\000\012\000\033\000\
\\016\000\032\000\017\000\031\000\018\000\030\000\000\000\
\\000\000\
\\002\000\041\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\042\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\043\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\045\000\003\000\002\000\004\000\044\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\049\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\002\000\051\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\052\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\053\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\054\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\055\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\056\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\057\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\058\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\059\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\060\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\061\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\062\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\063\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\018\000\064\000\000\000\
\\000\000\
\\000\000\
\\012\000\065\000\000\000\
\\000\000\
\\010\000\066\000\011\000\034\000\012\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\077\000\007\000\076\000\000\000\
\\002\000\079\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\081\000\003\000\002\000\005\000\080\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\045\000\003\000\002\000\004\000\083\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\088\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\089\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\090\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\002\000\091\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\099\000\000\000\
\\002\000\103\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\014\000\106\000\015\000\105\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\110\000\000\000\
\\002\000\111\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\113\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\115\000\015\000\105\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\120\000\003\000\002\000\008\000\001\000\000\000\
\\002\000\121\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\122\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\125\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\015\000\128\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\131\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\133\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\135\000\003\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 137
val numrules = 65
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | func_decl of unit ->  (A.fundec)
 | func_decls of unit ->  (A.fundec list)
 | var_decl of unit ->  (A.dec) | type_field of unit ->  (A.field)
 | type_fields of unit ->  (A.field list)
 | type_val of unit ->  (A.ty)
 | type_decl of unit ->  ({ name:A.symbol,pos:pos,ty:A.ty } )
 | type_decls of unit ->  ({ name:A.symbol,pos:pos,ty:A.ty }  list)
 | decl of unit ->  (A.dec) | decl_seq of unit ->  (A.dec list)
 | lvalue of unit ->  (A.var)
 | field of unit ->  ( ( A.symbol * A.exp * pos ) )
 | field_list of unit ->  ( ( A.symbol * A.exp * pos )  list)
 | expr_list of unit ->  (A.exp list)
 | expr_seq of unit ->  ( ( A.exp * pos )  list)
 | binary_expr of unit ->  (A.exp) | expr of unit ->  (A.exp)
 | program of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 32) => true | (T 33) => true | (T 34) => true | (T 40) => true
 | (T 36) => true | (T 37) => true | (T 38) => true | (T 42) => true
 | (T 43) => true | (T 44) => true | (T 28) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 35) => true | (T 39) => true
 | (T 41) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 31))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "UMINUS"
  | (T 25) => "AND"
  | (T 26) => "OR"
  | (T 27) => "ASSIGN"
  | (T 28) => "ARRAY"
  | (T 29) => "IF"
  | (T 30) => "THEN"
  | (T 31) => "ELSE"
  | (T 32) => "WHILE"
  | (T 33) => "FOR"
  | (T 34) => "TO"
  | (T 35) => "DO"
  | (T 36) => "LET"
  | (T 37) => "IN"
  | (T 38) => "END"
  | (T 39) => "OF"
  | (T 40) => "BREAK"
  | (T 41) => "NIL"
  | (T 42) => "FUNCTION"
  | (T 43) => "VAR"
  | (T 44) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.expr expr1, expr1left, expr1right)) :: 
rest671)) => let val  result = MlyValue.program (fn _ => let val  (
expr as expr1) = expr1 ()
 in (expr)
end)
 in ( LrTable.NT 0, ( result, expr1left, expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left),
 STRING1right)) :: rest671)) => let val  result = MlyValue.expr (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING, STRINGleft))
end)
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.IntExp(INT))
end)
 in ( LrTable.NT 1, ( result, INT1left, INT1right), rest671)
end
|  ( 3, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => (A.NilExp))
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.lvalue lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.expr (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 in (A.VarExp(lvalue))
end)
 in ( LrTable.NT 1, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in (
A.OpExp{left = A.IntExp(0), oper = A.MinusOp, right = expr, pos = MINUSleft}
)
end)
 in ( LrTable.NT 1, ( result, MINUS1left, expr1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.binary_expr binary_expr1, binary_expr1left, 
binary_expr1right)) :: rest671)) => let val  result = MlyValue.expr
 (fn _ => let val  (binary_expr as binary_expr1) = binary_expr1 ()
 in (binary_expr)
end)
 in ( LrTable.NT 1, ( result, binary_expr1left, binary_expr1right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, (lvalueleft as lvalue1left), _)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => let val  (lvalue as 
lvalue1) = lvalue1 ()
 val  (expr as expr1) = expr1 ()
 in (A.AssignExp({var = lvalue, exp = expr, pos = lvalueleft}))
end)
 in ( LrTable.NT 1, ( result, lvalue1left, expr1right), rest671)
end
|  ( 8, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr_list 
expr_list1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left)
, _)) :: rest671)) => let val  result = MlyValue.expr (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (expr_list as expr_list1) = expr_list1 ()
 in (
A.CallExp{func = Symbol.symbol(ID), args = rev(expr_list), pos = IDleft}
)
end)
 in ( LrTable.NT 1, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr_seq 
expr_seq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expr (fn _ => let val  (expr_seq as expr_seq1)
 = expr_seq1 ()
 in (A.SeqExp(rev(expr_seq)))
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.field_list 
field_list1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left
), _)) :: rest671)) => let val  result = MlyValue.expr (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (field_list as field_list1) = field_list1 ()
 in (
A.RecordExp{fields = rev(field_list), typ = Symbol.symbol(ID), pos = IDleft}
)
end)
 in ( LrTable.NT 1, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: _ :: (
 _, ( MlyValue.expr expr1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (
IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  (ID as ID1) = ID1 ()
 val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.ArrayExp{typ = Symbol.symbol(ID), size = expr1, init = expr2, pos = IDleft}
)
end)
 in ( LrTable.NT 1, ( result, ID1left, expr2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  expr1 =
 expr1 ()
 val  expr2 = expr2 ()
 in (A.IfExp{test=expr1, then'=expr2, else'=NONE, pos=IFleft})
end)
 in ( LrTable.NT 1, ( result, IF1left, expr2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.expr expr3, _, expr3right)) :: _ :: ( _, ( 
MlyValue.expr expr2, _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _))
 :: ( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result
 = MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 val  expr3 = expr3 ()
 in (A.IfExp{test=expr1, then'=expr2, else'=SOME expr3, pos=IFleft})

end)
 in ( LrTable.NT 1, ( result, IF1left, expr3right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _)
) :: rest671)) => let val  result = MlyValue.expr (fn _ => let val  
expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (A.WhileExp{test = expr1, body = expr2, pos = WHILEleft})
end)
 in ( LrTable.NT 1, ( result, WHILE1left, expr2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.expr expr3, _, expr3right)) :: _ :: ( _, ( 
MlyValue.expr expr2, _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _))
 :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as 
FOR1left), _)) :: rest671)) => let val  result = MlyValue.expr (fn _
 => let val  (ID as ID1) = ID1 ()
 val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 val  expr3 = expr3 ()
 in (
A.ForExp{var = Symbol.symbol(ID), escape = ref true, lo = expr1, hi = expr2, body = expr3, pos = FORleft}
)
end)
 in ( LrTable.NT 1, ( result, FOR1left, expr3right), rest671)
end
|  ( 16, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => (
A.BreakExp(BREAKleft)))
 in ( LrTable.NT 1, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 17, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.expr_seq 
expr_seq1, _, _)) :: _ :: ( _, ( MlyValue.decl_seq decl_seq1, _, _))
 :: ( _, ( _, (LETleft as LET1left), _)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => let val  (decl_seq as decl_seq1) = 
decl_seq1 ()
 val  (expr_seq as expr_seq1) = expr_seq1 ()
 in (
A.LetExp{decs = rev(decl_seq), body = A.SeqExp(rev(expr_seq)), pos = LETleft}
)
end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp{left = expr1, oper = A.PlusOp, right = expr2, pos = expr1left}
)
end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp{left = expr1, oper = A.MinusOp, right = expr2, pos = expr1left}
)
end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp{left = expr1, oper = A.TimesOp, right = expr2, pos = expr1left}
)
end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp{left = expr1, oper = A.DivideOp, right = expr2, pos = expr1left}
)
end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp{left = expr1, oper = A.EqOp, right = expr2, pos = expr1left})

end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp{left = expr1, oper = A.NeqOp, right = expr2, pos = expr1left})

end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp{left = expr1, oper = A.LtOp, right = expr2, pos = expr1left})

end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp{left = expr1, oper = A.GtOp, right = expr2, pos = expr1left})

end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp{left = expr1, oper = A.GeOp, right = expr2, pos = expr1left})

end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp{left = expr1, oper = A.LeOp, right = expr2, pos = expr1left})

end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.IfExp{test = expr1, then'=expr2, else' = SOME(A.IntExp(0)), pos = expr1left}
)
end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.binary_expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.IfExp{test = expr1, then'=A.IntExp(1), else'=SOME(expr2), pos=expr1left}
)
end)
 in ( LrTable.NT 2, ( result, expr1left, expr2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.expr expr1, exprleft, expr1right)) :: _ :: 
( _, ( MlyValue.expr_seq expr_seq1, expr_seq1left, _)) :: rest671)) =>
 let val  result = MlyValue.expr_seq (fn _ => let val  (expr_seq as 
expr_seq1) = expr_seq1 ()
 val  (expr as expr1) = expr1 ()
 in ((expr, exprleft)::expr_seq)
end)
 in ( LrTable.NT 3, ( result, expr_seq1left, expr1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.expr expr1, (exprleft as expr1left), 
expr1right)) :: rest671)) => let val  result = MlyValue.expr_seq (fn _
 => let val  (expr as expr1) = expr1 ()
 in ((expr, exprleft)::nil)
end)
 in ( LrTable.NT 3, ( result, expr1left, expr1right), rest671)
end
|  ( 32, ( rest671)) => let val  result = MlyValue.expr_seq (fn _ => (
nil))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 33, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.expr_list expr_list1, expr_list1left, _)) :: rest671)) => let
 val  result = MlyValue.expr_list (fn _ => let val  (expr_list as 
expr_list1) = expr_list1 ()
 val  (expr as expr1) = expr1 ()
 in (expr::expr_list)
end)
 in ( LrTable.NT 4, ( result, expr_list1left, expr1right), rest671)

end
|  ( 34, ( ( _, ( MlyValue.expr expr1, expr1left, expr1right)) :: 
rest671)) => let val  result = MlyValue.expr_list (fn _ => let val  (
expr as expr1) = expr1 ()
 in (expr::nil)
end)
 in ( LrTable.NT 4, ( result, expr1left, expr1right), rest671)
end
|  ( 35, ( rest671)) => let val  result = MlyValue.expr_list (fn _ =>
 (nil))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 36, ( ( _, ( MlyValue.field field1, _, field1right)) :: _ :: ( _,
 ( MlyValue.field_list field_list1, field_list1left, _)) :: rest671))
 => let val  result = MlyValue.field_list (fn _ => let val  (
field_list as field_list1) = field_list1 ()
 val  (field as field1) = field1 ()
 in (field::field_list)
end)
 in ( LrTable.NT 5, ( result, field_list1left, field1right), rest671)

end
|  ( 37, ( ( _, ( MlyValue.field field1, field1left, field1right)) :: 
rest671)) => let val  result = MlyValue.field_list (fn _ => let val  (
field as field1) = field1 ()
 in (field::nil)
end)
 in ( LrTable.NT 5, ( result, field1left, field1right), rest671)
end
|  ( 38, ( rest671)) => let val  result = MlyValue.field_list (fn _ =>
 (nil))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 39, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.field (fn _ => let val  (ID as ID1) = ID1 ()
 val  (expr as expr1) = expr1 ()
 in ((Symbol.symbol(ID), expr, IDleft))
end)
 in ( LrTable.NT 6, ( result, ID1left, expr1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID
 as ID1) = ID1 ()
 in (A.SimpleVar(Symbol.symbol(ID), IDleft))
end)
 in ( LrTable.NT 7, ( result, ID1left, ID1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, (lvalueleft as lvalue1left), _)) :: rest671))
 => let val  result = MlyValue.lvalue (fn _ => let val  (lvalue as 
lvalue1) = lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in (A.FieldVar(lvalue, Symbol.symbol(ID), lvalueleft))
end)
 in ( LrTable.NT 7, ( result, lvalue1left, ID1right), rest671)
end
|  ( 42, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: _ :: ( _, ( MlyValue.lvalue lvalue1, (lvalueleft as 
lvalue1left), _)) :: rest671)) => let val  result = MlyValue.lvalue
 (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (expr as expr1) = expr1 ()
 in (A.SubscriptVar(lvalue, expr, lvalueleft))
end)
 in ( LrTable.NT 7, ( result, lvalue1left, RBRACK1right), rest671)
end
|  ( 43, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (expr as expr1) = expr1 ()
 in (
A.SubscriptVar(A.SimpleVar(Symbol.symbol(ID), IDleft), expr, IDleft))

end)
 in ( LrTable.NT 7, ( result, ID1left, RBRACK1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.type_decls type_decls1, type_decls1left, 
type_decls1right)) :: rest671)) => let val  result = MlyValue.decl (fn
 _ => let val  (type_decls as type_decls1) = type_decls1 ()
 in (A.TypeDec(rev(type_decls)))
end)
 in ( LrTable.NT 9, ( result, type_decls1left, type_decls1right), 
rest671)
end
|  ( 45, ( ( _, ( MlyValue.var_decl var_decl1, var_decl1left, 
var_decl1right)) :: rest671)) => let val  result = MlyValue.decl (fn _
 => let val  (var_decl as var_decl1) = var_decl1 ()
 in (var_decl)
end)
 in ( LrTable.NT 9, ( result, var_decl1left, var_decl1right), rest671)

end
|  ( 46, ( ( _, ( MlyValue.func_decls func_decls1, func_decls1left, 
func_decls1right)) :: rest671)) => let val  result = MlyValue.decl (fn
 _ => let val  (func_decls as func_decls1) = func_decls1 ()
 in (A.FunctionDec(rev(func_decls)))
end)
 in ( LrTable.NT 9, ( result, func_decls1left, func_decls1right), 
rest671)
end
|  ( 47, ( ( _, ( MlyValue.decl decl1, _, decl1right)) :: ( _, ( 
MlyValue.decl_seq decl_seq1, decl_seq1left, _)) :: rest671)) => let
 val  result = MlyValue.decl_seq (fn _ => let val  (decl_seq as 
decl_seq1) = decl_seq1 ()
 val  (decl as decl1) = decl1 ()
 in (decl::decl_seq)
end)
 in ( LrTable.NT 8, ( result, decl_seq1left, decl1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.decl decl1, decl1left, decl1right)) :: 
rest671)) => let val  result = MlyValue.decl_seq (fn _ => let val  (
decl as decl1) = decl1 ()
 in (decl::nil)
end)
 in ( LrTable.NT 8, ( result, decl1left, decl1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.type_decl type_decl1, _, type_decl1right))
 :: ( _, ( MlyValue.type_decls type_decls1, type_decls1left, _)) :: 
rest671)) => let val  result = MlyValue.type_decls (fn _ => let val  (
type_decls as type_decls1) = type_decls1 ()
 val  (type_decl as type_decl1) = type_decl1 ()
 in (type_decl::type_decls)
end)
 in ( LrTable.NT 10, ( result, type_decls1left, type_decl1right), 
rest671)
end
|  ( 50, ( ( _, ( MlyValue.type_decl type_decl1, type_decl1left, 
type_decl1right)) :: rest671)) => let val  result = 
MlyValue.type_decls (fn _ => let val  (type_decl as type_decl1) = 
type_decl1 ()
 in (type_decl::nil)
end)
 in ( LrTable.NT 10, ( result, type_decl1left, type_decl1right), 
rest671)
end
|  ( 51, ( ( _, ( MlyValue.type_val type_val1, _, type_val1right)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (TYPEleft as 
TYPE1left), _)) :: rest671)) => let val  result = MlyValue.type_decl
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  (type_val as type_val1) = type_val1 ()
 in ({name = Symbol.symbol(ID), ty = type_val, pos = TYPEleft})
end)
 in ( LrTable.NT 11, ( result, TYPE1left, type_val1right), rest671)

end
|  ( 52, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.type_val (fn _ => let val  (ID
 as ID1) = ID1 ()
 in (A.NameTy(Symbol.symbol(ID), IDleft))
end)
 in ( LrTable.NT 12, ( result, ID1left, ID1right), rest671)
end
|  ( 53, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.type_fields 
type_fields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.type_val (fn _ => let val  (type_fields as 
type_fields1) = type_fields1 ()
 in (A.RecordTy(rev(type_fields)))
end)
 in ( LrTable.NT 12, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 54, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (
ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = 
MlyValue.type_val (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy(Symbol.symbol(ID), ARRAYleft))
end)
 in ( LrTable.NT 12, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.type_field type_field1, type_field1left, 
type_field1right)) :: rest671)) => let val  result = 
MlyValue.type_fields (fn _ => let val  (type_field as type_field1) = 
type_field1 ()
 in (type_field::nil)
end)
 in ( LrTable.NT 13, ( result, type_field1left, type_field1right), 
rest671)
end
|  ( 56, ( ( _, ( MlyValue.type_field type_field1, _, type_field1right
)) :: _ :: ( _, ( MlyValue.type_fields type_fields1, type_fields1left,
 _)) :: rest671)) => let val  result = MlyValue.type_fields (fn _ =>
 let val  (type_fields as type_fields1) = type_fields1 ()
 val  (type_field as type_field1) = type_field1 ()
 in (type_field::type_fields)
end)
 in ( LrTable.NT 13, ( result, type_fields1left, type_field1right), 
rest671)
end
|  ( 57, ( rest671)) => let val  result = MlyValue.type_fields (fn _
 => (nil))
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.type_field (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
{name = Symbol.symbol(ID1), escape = (ref true), typ = Symbol.symbol(ID2), pos = IDleft}
)
end)
 in ( LrTable.NT 14, ( result, ID1left, ID2right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: 
rest671)) => let val  result = MlyValue.var_decl (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (expr as expr1) = expr1 ()
 in (
A.VarDec{name = Symbol.symbol(ID), escape=(ref true), typ = NONE, init=expr, pos = VARleft}
)
end)
 in ( LrTable.NT 15, ( result, VAR1left, expr1right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, (VARleft as VAR1left), _)) :: rest671)) => let val  
result = MlyValue.var_decl (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (expr as expr1) = expr1 ()
 in (
A.VarDec{name = Symbol.symbol(ID1), escape=(ref true), typ = SOME (Symbol.symbol(ID2), ID2left), init = expr, pos = VARleft}
)
end)
 in ( LrTable.NT 15, ( result, VAR1left, expr1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.func_decl func_decl1, _, func_decl1right))
 :: ( _, ( MlyValue.func_decls func_decls1, func_decls1left, _)) :: 
rest671)) => let val  result = MlyValue.func_decls (fn _ => let val  (
func_decls as func_decls1) = func_decls1 ()
 val  (func_decl as func_decl1) = func_decl1 ()
 in (func_decl::func_decls)
end)
 in ( LrTable.NT 16, ( result, func_decls1left, func_decl1right), 
rest671)
end
|  ( 62, ( ( _, ( MlyValue.func_decl func_decl1, func_decl1left, 
func_decl1right)) :: rest671)) => let val  result = 
MlyValue.func_decls (fn _ => let val  (func_decl as func_decl1) = 
func_decl1 ()
 in (func_decl::nil)
end)
 in ( LrTable.NT 16, ( result, func_decl1left, func_decl1right), 
rest671)
end
|  ( 63, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: _ :: (
 _, ( MlyValue.type_fields type_fields1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left),
 _)) :: rest671)) => let val  result = MlyValue.func_decl (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (type_fields as type_fields1) = type_fields1 ()
 val  (expr as expr1) = expr1 ()
 in (
{name = Symbol.symbol(ID), params = type_fields, result = NONE, body = expr, pos = FUNCTIONleft}
)
end)
 in ( LrTable.NT 17, ( result, FUNCTION1left, expr1right), rest671)

end
|  ( 64, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.type_fields
 type_fields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, (
 _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  
result = MlyValue.func_decl (fn _ => let val  ID1 = ID1 ()
 val  (type_fields as type_fields1) = type_fields1 ()
 val  ID2 = ID2 ()
 val  (expr as expr1) = expr1 ()
 in (
{name = Symbol.symbol(ID1), params = type_fields, result = SOME (Symbol.symbol(ID2), ID2left), body = expr, pos = FUNCTIONleft}
)
end)
 in ( LrTable.NT 17, ( result, FUNCTION1left, expr1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
