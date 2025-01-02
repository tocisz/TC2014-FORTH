cold
\ flags
1 constant accept
2 constant marker1
3 constant marker2

variable S0
variable S1
variable S7

: handleMCR ( key - state flag )
	dup 109 = if \ m
		drop
		S1 @ 0
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
		dup @ ( dptr state )
		swap 2+ c@ ( state flag )
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
		S1 @ 0
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
0 c,

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
here 3 + , \ next state
marker1 c,

' rDigitLoop cfa ,
here 4 + , \ next state
44 c, \ ,
0 c,

' rDigit cfa ,
here 3 + , \ next state
marker2 c,

' rDigitLoop cfa ,
S0 @ , \ next state
41 c, \ )
accept c,

here S7 !
' rCR cfa ,

variable ringBuf 16 allot
here constant ringBufEnd
variable ringBufPtr
ringBuf ringBufPtr !

variable scratch 10 allot

: ringBufWrite ( key - pos )
	ringBufPtr @
	c!
	ringBufPtr @ dup 1+ ( pos pos+1 )
	dup ringBufEnd = if
		\ wrap
		drop ringBuf
	then ( pos pos+1 )
	ringBufPtr ! ( pos )
;

: csmove ( begin end offset - len )
	>r ( begin end R:offset )
	2dup ( begin end begin end )
	over - ( begin end begin len )
	scratch r> + swap ( begin end src dst len )
	cmove ( begin end )
	swap - ( len )
;

: copyToScratch ( begin end - len )
	2dup < if ( begin end )
		0 csmove
	else ( begin end )
		\ two cmoves needed
		swap ringBufEnd ( end begin end' )
		0 csmove ( end len1 )
		>r >r ( R:len1 end )
		ringBuf r> ( rb0 end R:len1 )
		2dup = not if
			r@ ( rb0 end len1 R:len1 )
			csmove ( len2 R:len1 )
		else
			2drop 0
		then
		r> + ( len )
	then ( len )
;

: parseInt ( pos len - n )
	over + 32 swap c! ( ensure termination )
	1- number
	drop
;

: storeInt ( pos len - )
	parseInt ,
;

variable mark1
variable mark2
variable handler \ called on recognized word
\ scan input according to an automaton, starting from a given state
\ calls handler when word is recognized
: scan ( state - )
	begin ( state )
		dup while \ state is non-zero
		key ( state key )
		dup ringBufWrite >r ( state key R:pos )
		swap dup 2+ swap ( key dptr state )
		@ execute ( state' flag )
		r> swap ( state' pos flag )
		dup marker1 = if
			over mark1 !
		then
		dup marker2 = if
			over mark2 !
		then
		accept = if ( state' pos )
			\ from mark1 to mark2
			mark1 @ mark2 @ ( state' pos mark1 mark2 )
			copyToScratch ( state' pos len )
			scratch swap 1- ( state' pos scratch len )
			storeInt ( state' pos )
			\ from mark2 acc position
			dup mark2 @ swap ( state' pos marker pos )
			copyToScratch ( state' pos len )
			scratch swap ( state' pos scratch len )
			storeInt ( state' pos )
			46 emit
		then ( state' pos )
		drop ( state' )
	repeat
;

variable nums
variable len
: msum
	0. ( sum )
	nums @ dup ( sum addr addr )
	len @ + swap ( sum end start )
	do
		i @
		i cell + @ ( msum a b )
		m* ( msum a*b )
		d+ ( msum' )
	[ 2 cells ] literal +loop
;

: scanMul ( - )
	0 len !
	here nums !
	S0 @ scan
	here nums @ - ( bytes )
	len !
	cr ." Sum of multiplications is... "
	msum d.
;
scanMul