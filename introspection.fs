( introspection )
variable wordranges
' lit cfa wordranges ! ( first word in the dictionary )
' task lfa @ pfa cfa , ( word before task is in ROM )
' task cfa , ( task is copied to RAM )

: ?exit if r> drop exit then ;

( will literal follow the word under address? )
: ?litword ( a - cfa/0 )
  @
  dup [ ' lit cfa ] literal = ?exit
  dup [ ' branch cfa ] literal = ?exit
  dup [ ' 0branch cfa ] literal = ?exit
  dup [ ' <loop> cfa ] literal = ?exit
  [ ' <+loop> cfa ] literal = ?exit
  0
;

( checks below expect cfa )
: ?forthword @ [ ' emit cfa @ ] literal = ;
: ?asmword dup 2+ swap @ = ;
: ?userword @ [ ' s0 cfa @ ] literal = ;
: ?constword @ [ ' 0 cfa @ ] literal = ;
: ?stringword @ [ ' <."> cfa ] literal = ;

: words2 context @ @ cr ( context gives NFA )
  begin dup id.
    pfa 2- ( cfa )
    dup ?forthword .
    dup ?asmword .
    dup ?constword .
    dup ?userword . cr
    2+ lfa @ ( go to previous word )
  dup 0= until
  drop
;

( conditions to continue introspection loop )
: ?sf ( a n - F x )
  swap @ swap ( w n )
  0 rot rot ( 1 w n )
  0= ( 1 w F )
  dup if ." ..." then
  ?exit ( exit if F != 0 )
  dup [ ' ;s cfa ] literal = ( 1 w F )
  dup if ." ;" then
  ?exit
  dup [ ' <;code> cfa ] literal = ( 1 w F )
  dup if ." ;<code> ..." then
  ?exit
  dup wordranges @ < ( 1 w F )
  dup if ." OOR1" then
  ?exit
  dup wordranges 2+ @ > ( 1 w F )
  swap ( 1 F1 w )
  wordranges 4 + @ < ( 1 F1 F2 )
  and dup ( 1 F F )
  dup if ." OOR2" then
  ?exit
  2drop 1 0 ( 1 0 )
;
: ?seeIt ( a n - F )
  ?sf drop
;

variable lastcompile
variable isimmediate

( print word definition )
: <see> ( pfa - )
  lastcompile 0 !
  dup cr ." : " nfa ( pfa nfa )
  dup id.
  c@ 64 and isimmediate ! ( pfa )
  dup cfa ?forthword if
    ( word defined by forth )
    200 ( a n )
    begin
      2dup ?seeIt lastcompile @ or while
      over ( a n a )
      @ 2+ nfa id. ( a n )
      over ?litword 0= not lastcompile @ not and if
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
      over @ [ ' compile cfa ] literal = if
        1 lastcompile !
      else
        0 lastcompile !
      then
      1- swap 2+ swap ( a' n-1 )
    repeat drop
  else
   ( word defined by asm )
   ." NATIVE"
  then drop
  isimmediate @ if ."  immediate" then
  cr
;

: see ' <see> ;
