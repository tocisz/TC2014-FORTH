cold
\ flags
1 constant accept
2 constant marker

variable S0
variable S1
variable S7

: handleMCR ( key - state flag )
	dup 109 = if \ m
		drop
		S1 @ marker
		exit
	then
	13 = if \ CR
		S7 @ 0
	else
		S0 @ 0
	then
;

\ args under dptr: 0) next state 2) letter to recognize 3) flags
\ letter in args leads to state in args
\ letter "m" leads to S1
\ CR leads to S7
\ any other leads to S0
: rChr ( key dptr - state flag )
	>r ( key R:dptr )
	\ dup emit
	dup r@ 2+ c@ = if ( key )
		\ recognized
		drop r> ( dptr )
		dup @ ( dptr state )
		swap 3 + c@ ( state flag )
		exit
	else
		rdrop
	then ( key )
	handleMCR
;

\ [0-9] leads to state in args
\ letter "m" leads to S1
\ CR leads to S7
\ any other leads to S0
: rDigit ( key dptr - state flag )
	>r ( key R:dptr )
	\ dup emit
	dup dup ( 3xkey )
	48 <
	swap 57 >
	or if \ not a digit
		rdrop
	else ( key )
		drop r> ( dptr )
		@ ( state )
		0 ( state flag )
		exit
	then ( key )
	handleMCR
;

\ like rChr but loop when key is digit
: rDigitLoop ( key dptr - state flag )
	over dup ( key dptr key key )
	48 <
	swap 57 >
	or if \ not a digit
		rChr
	else
		swap drop ( dptr )
		2- \ this state
		0 \ no flags
	then
;

\ args under dptr: none
\ letter "m" leads to S1
\ CR returns state 0
\ any other leads to S0
: rCR ( key dptr - state flag )
	drop ( key )
	dup 109 = if \ m
		S1 @ marker
		drop exit
	then
	13 = if \ CR
		0 0
	else
		S0 @ 0
	then
;

\ automaton for `mul([0-9]+,[0-9]+)` (accept)
\ and `\n\n` (terminate)
here S0 !
' rChr cfa ,
here 4 + , \ next state
109 c, \ m
marker c, \ mark begin

here S1 !
' rChr cfa ,
here 4 + , \ next state
117 c, \ u
0 c,

' rChr cfa ,
here 4 + , \ next state
108 c, \ l
0 c,

' rChr cfa ,
here 4 + , \ next state
40 c, \ (
0 c,

' rDigit cfa ,
here 2+ , \ next state

' rDigitLoop cfa ,
here 4 + , \ next state
44 c, \ ,
0 c,

' rDigit cfa ,
here 2+ , \ next state

' rDigitLoop cfa ,
S0 @ , \ next state
41 c, \ )
accept c,

here S7 !
' rCR cfa ,

variable beg
variable handler \ called on recognized word
variable position
\ scan input according to an automaton, starting from a given state
\ calls handler when word is recognized
: scan ( state - )
	0 position !
	begin ( state )
		dup while \ state is non-zero
		key ( state key )
		swap dup 2+ swap ( key dptr state )
		@ execute ( state' flag )
		dup marker = if
			position @ beg !
		then
		accept = if
			position @ beg @
			handler @
			execute
		then ( state' )
		1 position +!
	repeat
;

: printHandler ( begin end - )
	cr ." mul at " . .
;
' printHandler cfa handler !

: scanMul ( - )
	S0 @ scan
;
scanMul