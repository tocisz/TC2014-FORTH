: cell+ 2+ ;
: cells 2 * ;
: cell 2 ;

: merge-step ( right mid left -- right mid+ left+ )
	over @ over @ < if ( *mid < *left )
		over @ >r ( right mid left r:*mid)
		2dup - ( r m l m-l )
		over dup cell+ ( r m l m-l l l+2 )
		rot ( r m l l l+2 m-l )
		cmover ( ranges overlap! )
		r> over ( r m l *mid l )
		!
		>r cell+ ( r m+2 )
		2dup = r> swap ( r m+2 l r=m+2 )
		if
			drop dup ( r r r )
		then ( r m+2 l )
	then
	cell+ ( r r r+2 | r m[+2] l+2 )
;

: merge ( right mid left -- right left )
	dup >r ( r m l rs:l )
	begin
		2dup > while
		merge-step
	repeat ( r m+ l+, where l+ >= m+ )
	2drop r> ( r l )
;

: mid ( l r -- mid )
	over - 2 /
	cell negate and
	+
;

: mergesort ( right left -- right left )
	2dup cell+ > not if
		exit ( if right <= left )
	then
	swap ( l r )
	2dup mid ( l r mid )
	recurse
	rot ( r mid l )
	recurse
	merge ( r l )
;

: sort ( addr len -- )
	cells over + swap mergesort 2drop
;

variable test
8 test ! 1 , 5 , 3 , 9 , 0 , 2 , 7 , 6 , 4 ,

: .array ( addr len -- )
	0 do
		dup i cells + @ .
	loop
	drop
;

( mergesort works, but.. )
( it's O(n^2 log n)
( so insertsort will be better )
test 10 2dup sort .array
