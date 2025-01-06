\ require 24d5.f

\ Observation: Page numbers are from 11 to 99
\ Observation: No more than 70 characters in a line
\ 2. Sort ordering by first number
\ 3. For each sequence
\ 3a. Check numbers in order
\ 3b. Maintain bitset of numbers in prefix (13 bytes)
\ 3c. If number must be followed by some other number, check whether it is in the prefix
\ 3d. If it is, reject the sequence

: mid ( l r -- mid )
	over - 2u/ ( l [r-l]/2 )
	-2 and ( round to cell size )
	+
;

: mid1 ( l r -- mid )
	over - 2u/ ( l [r-l]/2 )
	+
;

\ compare like >, but ignore high 8 bits
: cmp ( v1 v2 - f )
	255 and
	swap 255 and
	u<
;

: binsearch ( l r val - addr )
	>r ( R: val)
	begin
		2dup swap - [ cell 1+ ] literal <
		if ( 2 element range )
			over @ r> cmp if
				drop
			else
				swap drop
			then
			exit
		then
		2dup mid ( l r mid )
		dup @ r@ cmp ( l r mid *mid>val )
		if
			swap drop ( l mid )
		else
			rot drop swap ( mid r )
		then
	again
;

: insertsort ( addr len -- )
	1 do
		dup i cells + ( l r=l+i )
		2dup dup @ ( l r l r *r )
		dup >r ( R: *r )
		binsearch ( l r pos )
		swap >r ( l pos R: *r r )
		dup 2dup cell+ swap r> swap - ( l pos pos pos+1 r-pos )
		cmove> ( l pos )
		r> swap ! ( l )
	loop
	drop
;

13 constant bitsetLength
variable bitset bitsetLength allot

variable (bitmask) cell negate allot
1 dup c, ( 1 )
2u* dup c, ( 2 )
2u* dup c, ( 4 )
2u* dup c, ( 8 )
2u* dup c, ( 16 )
2u* dup c, ( 32 )
2u* dup c, ( 64 )
2u* c, ( 128 )

: bitmask ( n - mask )
	7 and
	(bitmask) + c@
;

: bsClear ( - )
	bitset bitsetLength erase
;

: 8u/
	2u/ 2u/ 2u/
;

: bsSet ( n - )
	dup 8u/ ( n n/8 )
	bitset + ( n addr )
	dup c@ ( n addr v )
	rot bitmask or ( addr v|mask )
	swap c!
;

: bsTest ( n - f )
	dup 8u/ ( n n/8 )
	bitset + ( n addr )
	c@ ( n v )
	swap bitmask and ( v&mask )
;

: succInit ( l r v - addr )
	binsearch
;

: succNext ( addr v - addr' succ 1 | addr' 0 )
	>r 2- ( addr' R: v )
	dup c@ ( addr' v' )
	r> - if
		0
	else
		dup 1+ c@ 1
	then
;

order
orderLength @
+ constant orderEnd

: .succ ( v - )
	>r
	order orderEnd
	r@ ( l r v )
	succInit ( addr )
	cr
	begin
		r@ succNext while
		.
	repeat
	rdrop drop
;
\ order orderLength @ 2u/ insertsort
\ order orderLength @ .pairsTab
\ 97 .succ ( 75 53 29 47 61 13 )

variable seqStart
variable failed
\ check whether succ(v) is in the prefix
\ if it is, set failed to 1
: checkOne ( v - )
	\ find numbers forbidden before v
	\ cr ." succ of " dup .
	>r
	order orderEnd
	r@ ( l r v )
	succInit ( addr )
	\ cr
	begin
		r@ succNext while ( addr' sv )
		\ check whether it is in the prefix
		\ dup .
		bsTest if
			1 failed !
			\ ." - "
			rdrop drop
			exit
		then
	repeat
	rdrop drop
;
variable midSum
: solve
	cr ." Sorting..."
	order orderLength @ 2u/ insertsort
	cr ." Checking sequences"
	sequences ( ptr )
	numOfSequences @ 0 do
		dup seqStart !
		0 failed !
		bsClear
		begin ( ptr )
			dup c@ ( ptr v )
			dup while \ non-zero
			failed @ if
				drop
			else
				\ should we allow repeating numbers?
				dup bsSet ( ptr v )
				checkOne ( ptr )
			then ( ptr )
			1+ ( ptr' )
		repeat
		drop
		failed @ if
			." ."
		else
			." *"
			seqStart @ over mid1 c@
			midSum +!
		then
		1+ ( ptr' )
	loop
	drop
	cr ." Sum of middle elements: " midSum @ u.
;

solve
