( introspection )
' emit cfa @ constant forthword
' s0 cfa @ constant userword
' 0 cfa @ constant constword
' ;s cfa constant stopword
' <."> cfa constant stringword
' compile cfa constant compileword

( TODO: compile above into ?words )

variable wordranges
' lit cfa wordranges !
' reset cfa , ( from lit to reset in ROM )
' task cfa ,

variable litwords
0
' lit cfa , 1+
' branch cfa , 1+
' 0branch cfa , 1+
' <loop> cfa , 1+
' <+loop> cfa , 1+
litwords ! ( size in var )

( will literal follow the word under address? )
: ?litword ( a - F )
  @ litwords @ 0 do
    dup i 1+ dup + litwords + @ = ( a F )
    if 0 leave then
  loop ( a | a 0)
  0= if
    drop 1
  else
    0
  then
;

( checks below expect cfa )
: ?forthword @ forthword = ;
: ?asmword dup 2+ swap @ = ;
: ?userword @ userword = ;
: ?constword @ constword = ;
: ?stringword @ stringword = ;

: words2 context @ @ cr ( context gives NFA )
  begin dup id.
    pfa 2- ( cfa )
    dup ?forthword .
    dup ?asmword .
    dup ?constword .
    dup ?userword . cr
    2+ lfa @ ( go to previous word )
  dup 0= until
;

: ?exit if r> drop exit then ;

( conditions to continue introspection loop )
( TODO: return reason of termination )
: ?sf ( a n - F x )
  swap @ swap ( w n )
  0 rot rot ( 1 w n )
  0= ( 1 w F )
  ?exit ( exit if F != 0 )
  dup stopword = ( 1 w F )
  ?exit
  dup wordranges @ < ( 1 w F )
  ?exit
  dup wordranges 2+ @ > ( 1 w F )
  swap ( 1 F1 w )
  wordranges 4 + @ < ( 1 F1 F2 )
  and dup ( 1 F F )
  ?exit
  2drop 1 0 ( 1 0 )
;
: ?seeIt ( a n - F )
  ?sf drop
;

variable lastcompile
variable isimmediate

( print word definition )
( TODO: ending should depend on reason for termination )
: see ( WORD - )
  ' ( pfa )
  lastcompile 0 !
  dup cr ." : " nfa ( pfa nfa )
  dup id.
  c@ 64 and isimmediate ! ( pfa )
  dup cfa ?forthword if
    ( word defined by forth )
    100 ( a n )
    begin
      2dup ?seeIt lastcompile @ or while
      over ( a n a )
      @ 2+ nfa id. ( a n )
      over ?litword lastcompile @ not and if
        1- swap 2+ ( n-1 a' )
        dup @ .
        swap ( a' n-1 )
      then ( a n )
      over ?stringword lastcompile @ not and if
        1- swap 2+ ( n-1 a' )
        count 2dup ( n-1 a'' c a'' c )
        type + 2- ( n-1 a''' )
        swap ( a''' n-1 )
        34 emit space
      then
      over @ compileword = if
        1 lastcompile !
      else
        0 lastcompile !
      then
      1- swap 2+ swap ( a' n-1 )
    repeat drop
  else
   ( word defined by asm )
   ." NATIVE"
  then drop ." ; "
  isimmediate @ if ." immediate" then
  cr
;
