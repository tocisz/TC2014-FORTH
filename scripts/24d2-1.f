( requires 24d2.f )

: b->s
	dup 128 and if
		-256 or
	then
;

: diffLine ( laddr - )
	dup >r ( laddr R:laddr )
	line@addr c@ ( v )
	r@ line@addr 1+ ( v start+1 )
	r> line@cnt 1- ( v start+1 cnt-1 )
	over + swap ( v end start+1 )
	do
		i c@ dup ( v v' v' )
		rot - ( v' v'-v )
		i c! ( v' )
	loop
	drop
;

: diffs ( caddr - )
	begin
		dup head diffLine
		tail
	dup 0= until
	drop
;

: sgn ( n - sgn )
	dup 0< if
		drop -1
	else 0= if
		0
	else
		1
	then then
;

: isSafe ( laddr - t/f )
	2 swap ( sgn laddr )
	dup >r ( 2 laddr R:laddr )
	line@addr 1+ ( 2 start+1 )
	r> line@cnt 1- ( 2 start+1 cnt-1 )
	over + swap ( 2 end start+1 )
	do ( sgn )
		i c@ b->s ( sgn v )
		over 2 = if ( sgn:2 v )
			dup >r ( 2 v R:v )
			sgn swap drop ( sgn )
			dup 0= if ( sgn:0 )
				rdrop ( drop v )
				rdrop rdrop ( drop loop counter )
				drop 0 ( not safe )
				exit
			then
			r> ( sgn v )
		else 2dup sgn = not if ( sgn different or 0 )
			rdrop rdrop ( drop loop counter )
			2drop 0 ( not safe )
			exit
		then then
		abs 3 > ( |v|>3 )
		if ( diff oor )
			rdrop rdrop ( drop loop counter )
			drop 0 ( not safe )
			exit
		then
	loop
	drop 1 ( safe )
;

variable safeCnt
: countSafe ( caddr - num )
	0 safeCnt !
	begin
		dup head isSafe
		safeCnt +!
		tail
	dup 0= until
	drop
	safeCnt @
;

: compute ( caddr - n )
	cr ." Calculating diffs..."
	dup diffs
	cr ." Counting safe sequences..."
	countSafe
	cr ." Answer is: " .
;

nums @ compute
