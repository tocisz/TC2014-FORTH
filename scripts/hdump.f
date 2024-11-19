: 4h. bswap swap bswap swap <# # # # # # # # # #> type ;
: 2h. bswap 0 <# # # # # #> type ;
: 1h. 0 <# # # #> type ;

( contol sum )
hex
: 4sum ( n m - s )
	over bswap over bswap
	+ + +
;

: 2sum ( n - s )
	dup
	bswap
	+
;

variable hdumps
: (hdump) ( a n - a+n )
	begin
		dup 3 > while
		swap ( n a )
		dup 2@ 4h.
		dup 2@ 4sum hdumps +!
		4 + ( n a+4 )
		swap 4 - ( a+4 n-4 )
	repeat
	dup 1 > if
		swap
		dup @ 2h.
		dup @ 2sum hdumps +!
		2+
		swap 2-
	then
	dup 0> if
		swap
		dup c@ 1h.
		dup c@ hdumps +!
		1+
		swap
	then
	drop
;

: hdump ( a n - )
	base @ rot rot
	hex
	cr
	begin
		dup 0> while
		3A emit ( start line )
		dup 10 min ( a n m )
		swap over ( a m n m )
		- swap ( a n-m m)
		dup 1h. ( print data length )
		dup hdumps ! ( start sum )
		rot dup bswap 2h. ( n-m m a : address )
		dup 2sum hdumps +! ( update sum )
		0 1h. ( record type )
		swap ( n-m a m )
		(hdump) ( n-m a+m : data )
		swap ( a+m n-m )
		100 hdumps @ FF and - 1h.
		cr
	repeat
	2drop
	base !
;
