\ requires 24d4.f

: tab@  ( x y - value )
	2dup
	dup 0 <
	swap cols @ 1- >
	or if ( x y x ) \ y<0 or y>=cols
		drop 2drop
		0 exit
	then
	dup 0 <
	swap rows @ 1- >
	or if ( x y ) \ x<0 or x>=rows
		2drop
		0 exit
	then
	cols @ * +
	tab @ +
	c@
;

variable dx
variable dy
variable x
variable y

: sinit ( x y dx dy - )
	dy ! dx ! y ! x !
;

: sstep ( - )
	x @ dx @ + x !
	y @ dy @ + y !
;

: sget ( - value )
	x @ y @ tab@
;

\ : prints ( - )
\ 	begin
\ 		sget ( v )
\ 		dup while \ v != 0
\ 		emit
\ 		sstep
\ 	repeat
\ ;

1 constant a:*
2 constant a:end

variable s:0
variable s:X
variable s:S

88 constant #X
77 constant #M
65 constant #A
83 constant #S

: rStart ( key dptr - state flag )
	drop
	dup #X = if \ X
		drop
		s:X @ 0
		exit
	then
	dup #S = if \ S
		drop
		s:S @ 0
		exit
	then
	0= if
		s:0 @ a:end
	else
		s:0 @ 0 \ stay in s:0
	then
;

\ args under dptr: 0) next state 2) letter to recognize 3) flags
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

\ automaton for `XMAS|SAMX` -> a:*
\ and `\0` -> a:end
here s:0 !
' rStart cfa , \ ignores dptr

here s:X !
' rChr cfa ,
here 4 + , \ next state
#M c,
0 c,

' rChr cfa ,
here 4 + , \ next state
#A c,
0 c,

' rChr cfa ,
here 4 + , \ next state
#S c,
a:* c,

here s:S !
' rChr cfa ,
here 4 + , \ next state
#A c,
0 c,

' rChr cfa ,
here 4 + , \ next state
#M c,
0 c,

' rChr cfa ,
s:X @ , \ back to X
#X c,
a:* c,

variable count

\ scan input according to an automaton
\ calls handler when word is recognized
: scan ( - )
	s:0 @ ( state )
	begin ( state )
		sget ( state key )
		sstep
		\ dup emit
		swap dup 2+ swap ( key dptr state )
		@ execute ( state' flag )
		dup a:* = if
			1 count +!
			42 emit
		then
	a:end = until
;

: cover
	0 count !
	\ -
	\ cr
	rows @ 0 do
		0 i
		1 0 sinit
		scan
		\ cr
	loop
	\ |
	\ cr
	cols @ 0 do
		i 0
		0 1 sinit
		scan
		\ cr
	loop
	\ \
	\ cr
	rows @ 3 - 0 do \ by rows
		0 i
		1 1 sinit
		scan
		\ cr
	loop
	cols @ 3 - 1 do \ by columns
		i 0
		1 1 sinit
		scan
		\ cr
	loop
	\ /
	\ cr
	rows @ 3 - 0 do \ by rows
		cols @ 1- i
		-1 1 sinit
		scan
		\ cr
	loop
	cols @ 1- 3 do \ by columns
		i 0
		-1 1 sinit
		scan
		\ cr
	loop
	cr ." count = " count @ .
;

cover
