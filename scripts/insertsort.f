: cell+ 2+ ;
: cells 2 * ;
: cell 2 ;

: mid ( l r -- mid )
	over - 2 / ( l [r-l]/2 )
	cell negate and ( round to cell size )
	+
;

: binsearch ( l r val - addr )
	begin
		>r
		2dup swap - 3 < if ( 2 element range )
			over @ r> > if
				drop
			else
				swap drop
			then
			exit
		then
		2dup mid
		dup @ r@ > ( l r mid *mid>val )
		if
			swap drop ( l mid )
		else
			rot drop swap ( mid r )
		then
		r>
	again
;

: insertsort ( addr len -- )
	over + over 1+ ( left right unsorted )

;

variable test
0 test ! 1 , 1 , 2 , 5 , 6 , 6 , 7 ,

: .array ( addr len -- )
	0 do
		dup i cells + @ .
	loop
	drop
;

test 8 .array

test dup 8 cells + 1 binsearch
@ . ( 2 )

test dup 8 cells + 2 binsearch
@ . ( 5 )

test dup 8 cells + 0 binsearch
@ . ( 1 )

test dup 8 cells + -1 binsearch
@ . ( 0 )

test dup 8 cells + 10 binsearch
dup test - 2 / . ( 8 -- out of array )
