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
: permutation ( n i - j )
	2dup swap 2u/ < if ( i<n/2 )
		2u*
	else
		over 1- swap -
		2u*
		over 1- swap -
	then
	swap drop
;

: .permutation ( n - )
	dup
	0 do
		dup i permutation .
	loop
	drop
;
10 .permutation

: .array ( addr len -- )
	0 do
		dup i cells + ?
	loop
	drop
;

( Let's see how cycles of the permutation look. )
variable cycleCnt
variable cycleTab
variable N
: cycles ( n - addr )
	dup N ! ( used by permutation )
	1 cycleCnt !
	here cycleTab !
	dup cells allot ( allocate mem )
	dup cells cycleTab @ swap erase ( fill with zeros )
	0 do
		cycleTab @ i cells + @ 0= if ( not visited )
			i dup ( start with i )
			begin
				dup >r cycleCnt @ cycleTab @ r> cells + ! ( mark as visited )
				N @ swap permutation ( next number in cycle )
			2dup = until ( back to the start )
			2drop
			1 cycleCnt +!
		then
	loop
	cycleTab @
;

: printCycles
	62 2 do
		cr ." n = " i .
		cr i dup dup cycles swap .array
		cells negate allot ( free memory )
	2 +loop
;

printCycles

( We could traverse whole permutation )
( and if it reaches index below current )
( than it was visited. )
( But that's probably O(n^2)
( Can we do better? )