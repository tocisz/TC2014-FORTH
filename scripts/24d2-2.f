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
	else
		0= not
	then
;

( this is useful only for first iteration )
: ?fail0 ( v - sgn t/f )
	dup sgn ( v sgn )
	dup 0= ( v sgn t/f )
	rot abs 3 > ( sgn t/f oor? )
	or ( sgn t/f )
;

: ?fail1 ( sgn v - sgn t/f )
	2dup sgn = not if ( sgn different or 0 )
		drop
		1 exit
	then
	abs 3 > ( oor? )
;

: loopRange ( offset laddr - end start+offset )
	dup >r ( offset laddr R:laddr )
	line@addr + ( start+offset )
	r@ line@addr r> line@cnt + ( start+offset end )
	swap ( end start+offset )
;

: .line ( laddr - )
	cr
	dup >r ( laddr R:laddr )
	line@addr ( start )
	r> line@cnt ( start cnt )
	over + swap ( end start )
	do
		i c@ b->s .
	loop
;

variable problemCnt
: isSafe ( laddr - t/f )
	( dup .line)
	0 problemCnt !
	dup >r ( laddr R:laddr )
	line@addr 1+ ( start+1 )
	c@ b->s ( v0 )
	( special test when sign is not known )
	?fail0 ( sgn fail0 )
	dup problemCnt +!
	if ( not safe )
		( cr ." 0 not safe ")
		drop
		r@ line@addr 2+ ( start+offset )
		( retry )
		c@ b->s ( v )
		( dup .)
		?fail0 ( sgn fail0 )
		dup problemCnt +!
		if ( not safe )
			( cr ." 1 not safe")
			rdrop drop
			0 exit
		then
		( .s)
		3
	else
		2
	then ( sgn offset )
	( loop diffs when sign is known )
	0 swap ( adj=0 )
	r> ( sgn adj offset laddr )
	loopRange ( sgn adj end start+offset )
	do ( sgn adj )
		i c@ b->s ( sgn adj v )
		+ ( sgn v' )
		?fail1 ( sgn fail1 )
		dup problemCnt +!
		if ( problem )
			i c@ b->s
		else
			0
		then ( sgn adj )
		problemCnt @ 1 > if ( terminate )
			leave
		then
	loop
	2drop
	problemCnt @ 2 <
;

variable safeCnt
: countSafe ( caddr - num )
	0 safeCnt !
	begin
		dup head isSafe
		safeCnt +!
		tail
		46 emit
	dup 0= until
	drop
	safeCnt @
;

: compute ( caddr - n )
	cr ." Calculating diffs..."
	dup diffs
	cr ." Counting safe sequences"
	countSafe
	cr ." Answer is: " .
;

nums @ compute
