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
variable canRetry
variable laddr
: innerLoop ( sgn adj offset laddr - sgn adj' )
	loopRange ( sgn adj end start+offset )
	do ( sgn adj )
		i c@ b->s ( sgn adj v )
		+ ( sgn v' )
		?fail1 ( sgn fail1 )
		dup problemCnt +!
		if ( problem )
			i c@ b->s
			." remove " dup .
		else
			0
		then ( sgn adj )
		problemCnt @ 1 > if ( terminate )
			leave
		then
	loop
;
: isSafe ( laddr - t/f )
	dup .line
	0 problemCnt !
	0 canRetry !
	dup laddr ! ( laddr )
	line@addr 1+ ( start+1 )
	c@ b->s ( v0 )
	( special test when sign is not known )
	?fail0 ( sgn fail0 )
	dup problemCnt +!
	if ( not safe )
		." try remove 1st "
		drop
		laddr @ line@addr 2+ ( start+offset )
		( retry )
		c@ b->s ( v )
		( dup .)
		?fail0 ( sgn fail0 )
		dup problemCnt +!
		if ( not safe )
			." remove 2nd "
		then
		( .s)
		3
	else
		2
	then ( sgn offset )
	( first possibility: )
	( no removal or 1st element removed )
	0 swap ( sgn adj:0 offset )
	( loop diffs when sign is known )
	laddr @ ( sgn adj offset laddr )
	problemCnt @
	dup 0> if
		1 canRetry !
	then
	2 < if ( sgn adj offset laddr )
		innerLoop
	else
		2drop
	then ( sgn adj' )
	2drop ( )
	cr problemCnt @ . ." problems "
	problemCnt @ 2 < if
		1 ( safe )
	else
		canRetry @ if
			cr ." try remove 2nd "
			( where to find laddr? )
			laddr @ line@addr dup 1+ ( a0 a1 )
			c@ b->s ( a0 v1 )
			swap 2+ c@ b->s ( v1 v2 )
			over + sgn ( v1 sgn[v1+v2])
			swap ( sgn adj )
			2 laddr @ ( sgn adj offset laddr )
			1 problemCnt !
			innerLoop
			2drop
		then
		cr problemCnt @ . ." problems "
		problemCnt @ 2 <
	then
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
