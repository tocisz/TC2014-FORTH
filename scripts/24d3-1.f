cold
\ flags
1 constant a:mul
2 constant a:m1
3 constant a:m2
4 constant a:end

variable s:0
variable s:m
variable s:CR

: rStart ( key dptr - state flag )
	drop
	dup 109 = if \ m
		drop
		s:m @ 0
		exit
	then
	13 = if \ CR
		s:CR @ 0
	else
		s:0 @ 0
	then
;

\ args under dptr: 0) next state 2) letter to recognize 3) flags
\ letter in args leads to state in args
\ letter "m" leads to s:m
\ CR leads to s:CR
\ any other leads to s:0
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
	0 rStart
;

: isNotDigit ( key - flag )
	dup
	48 <
	swap 57 >
	or
;

\ [0-9] leads to state in args
\ letter "m" leads to s:m
\ CR leads to s:CR
\ any other leads to s:0
: rDigit ( key dptr - state flag )
	>r ( key R:dptr )
	dup isNotDigit if \ not a digit
		rdrop
	else ( key )
		drop r> ( dptr )
		dup @ ( dptr state )
		swap 2+ c@ ( state flag )
		exit
	then ( key )
	0 rStart
;

\ like rChr but loop when key is digit
: rDigitLoop ( key dptr - state flag )
	over isNotDigit if \ not a digit
		rChr
	else
		swap drop ( dptr )
		2- \ this state
		0 \ no flags
	then
;

\ automaton for `mul([0-9]+,[0-9]+)` -> a:mul
\ and `\n\n` -> a:end
here s:0 !
' rStart cfa , \ ignores dptr

here s:m !
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
a:m1 c,

' rDigitLoop cfa ,
here 4 + , \ next state
44 c, \ ,
0 c,

' rDigit cfa ,
here 3 + , \ next state
a:m2 c,

' rDigitLoop cfa ,
s:0 @ , \ next state
41 c, \ )
a:mul c,

here s:CR !
' rChr cfa ,
s:CR @ , \ loop
13 c, \ CR
a:end c,

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

: handleAction ( pos flag - pos flag )
	dup a:m1 = if
		over mark1 !
		exit
	then
	dup a:m2 = if
		over mark2 !
		exit
	then
	dup a:mul = if ( pos flag )
		>r ( pos R:flag )
		\ from mark1 to mark2
		mark1 @ mark2 @ ( pos mark1 mark2 )
		copyToScratch ( pos len )
		scratch swap 1- ( pos scratch len )
		storeInt ( pos )
		\ from mark2 acc position
		dup mark2 @ swap ( pos marker pos )
		copyToScratch ( pos len )
		scratch swap ( pos scratch len )
		storeInt ( pos )
		46 emit
		r> ( pos flag )
	then ( pos flag )
;

\ scan input according to an automaton, starting from a given state
\ calls handler when word is recognized
: scan ( state - )
	begin ( state )
		key ( state key )
		dup ringBufWrite >r ( state key R:pos )
		swap dup 2+ swap ( key dptr state )
		@ execute ( state' flag )
		r> swap ( state' pos flag )
		dup if
			handleAction
		then ( state' pos flag )
		swap drop ( state' flag )
	a:end = until
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
	s:0 @ scan
	here nums @ - ( bytes )
	len !
	cr ." Sum of multiplications is... "
	msum d.
;
scanMul