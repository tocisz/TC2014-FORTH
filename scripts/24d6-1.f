\ requires: 24d6.f

: tab@ ( x y - value )
	cols @ * +
	tab +
	c@
;

: tab! ( value x y - )
	cols @ * +
	tab +
	c!
;

: cktab@  ( x y - value )
	2dup
	dup 0 <
	swap cols @ 1- >
	or if ( x y x ) \ y<0 or y>=cols
		drop 2drop
		-1 exit
	then
	dup 0 <
	swap rows @ 1- >
	or if ( x y ) \ x<0 or x>=rows
		2drop
		-1 exit
	then
	tab@
;

\ 88 constant 'X'
\ : .tab ( - )
\ 	rows @ 0 do
\ 		cr
\ 		cols @ 0 do
\ 			i j tab@
\ 			dup 1 and if
\ 				drop
\ 				'#'
\ 			else
\ 				2 and if
\ 					'X'
\ 				else
\ 					'.'
\ 				then
\ 			then
\ 			emit
\ 		loop
\ 	loop
\ ;

variable direction

\ 0 ^
\ 1 >
\ 2 v
\ 3 <
: step ( - )
	direction @
	dup 0= if
		drop
		-1 y +!
	else
		dup 1 = if
			drop
			1 x +!
		else
			2 = if
				1 y +!
			else
				-1 x +!
			then
		then
	then
;

: stepback ( - )
	direction @
	dup 0= if
		drop
		1 y +!
	else
		dup 1 = if
			drop
			-1 x +!
		else
			2 = if
				-1 y +!
			else
				1 x +!
			then
		then
	then
;

: rotate
	direction @
	1+
	dup 4 = if
		drop 0
	then
	direction !
;

: mark ( - )
	2 x @ y @ tab!
;

variable sx
variable sy
: inLoop ( - f )
	direction @ if
		\ direction is not up
		0 exit
	then
	sx @ x @ - if
		\ x is not starting x
		0 exit
	then
	sy @ y @ = \ y is starting y
;

: walk ( - )
	0 direction !
	begin
		inLoop if
			exit
		then
		x @ y @ cktab@ ( v )
		dup 0< if
			drop
			exit
		then ( v )
		1 and if ( v )
			stepback
			rotate
		else
			mark
		then
		step
	again
;

variable cnt
: count ( - )
	0 cnt !
	rows @ 0 do
		cols @ 0 do
			i j tab@
			2 and if
				1 cnt +!
			then
		loop
	loop
	cnt @
;

: solve
	walk
	\ .tab
	cr ." count = " count .
;
solve
