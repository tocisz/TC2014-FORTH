: cell+ 2+ ;
: cells 2u* ;
: cell 2 ;

: mid ( l r -- mid )
	over - 2u/ ( l [r-l]/2 )
	cell negate and ( round to cell size )
	+
;

( find position of a first value greater than val )
: binsearch ( l r val - addr )
	>r
	begin
		2dup swap - [ cell 1+ ] literal <
		if ( 2 element range )
			over @ r> > if
				drop
			else
				swap drop
			then
			exit
		then
		2dup mid ( l r mid )
		dup @ r@ > ( l r mid *mid>val )
		if
			swap drop ( l mid )
		else
			rot drop swap ( mid r )
		then
	again
;

: insertsort ( addr len -- )
	over + over cell+ ( left right unsorted )
	dup @ ( left right unsorted *unsorted )

;

variable test
0 test ! 1 , 1 , 2 , 5 , 6 , 6 , 7 ,

: .array ( addr len -- )
	0 do
		dup i cells + ?
	loop
	drop
;

test 8 .array

: test1 >r test dup 8 cells + r> binsearch ;
-1 test1 ? ( 0 )
0 test1 ? ( 1 )
1 test1 ? ( 2 )
2 test1 ? ( 5 )
6 test1 ? ( 7 )

10 test1
dup test - 2u/ . ( 8 -- out of array )
