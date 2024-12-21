( Suppose we have 2 arrays interleaved in memory )
( ABABAB )
( and we want to unzip it )
( AAABBB )

( Permuting it by the inverse of this permutation )
( will do the job. )
( To do this we need to traverse all cycles )
( of the permutation and shift elements within cycles. )
( But each cycle needs to be traversed once. )
( How to do this w/o additional memory? )
variable N
: permutation ( i - j )
	dup N @ 2u/ < if
		2u*
	else
		N @ 1- swap -
		2u*
		N @ 1- swap -
	then
;

10 N !
: .permutation
	N @ 0 do
		i permutation .
	loop
;
.permutation

: .array ( addr len -- )
	0 do
		dup i cells + ?
	loop
	drop
;

( Let's see how cycles of the permutation look. )
variable rank
variable cycleTab
: cycles ( n - addr )
	dup N ! ( used by permutation )
	1 rank !
	here cycleTab !
	dup cells allot ( allocate mem )
	dup cells cycleTab @ swap erase ( fill with zeros )
	0 do
		cycleTab @ i cells + @ 0= if ( not visited )
			i dup ( start with i )
			begin
				dup >r rank @ cycleTab @ r> cells + ! ( mark as visited )
				permutation ( next number in cycle )
			2dup = until ( back to the start )
			2drop
			1 rank +!
		then
	loop
	cycleTab @
;

: printCycles
	62 2 do
		cr i .
		cr
		i dup dup cycles swap .array
		cells negate allot ( free memory )
	2 +loop
;

printCycles
