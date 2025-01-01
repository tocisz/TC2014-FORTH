( requires 24d2.f )

: b->s
	dup 128 and if
		-256 or
	then
;

variable laddr
: laddrRange ( - end start+1 )
	laddr @ dup ( laddr laddr )
	line@addr dup rot ( start start laddr )
	line@cnt + ( start end )
	swap 1+ ( end start+1 )
;

: diffLine ( laddr - )
	dup laddr !
	line@addr c@ ( v )
	laddrRange do
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


variable direction
: detectDirection ( - n )
	0 direction !
	laddrRange do
		i c@ b->s
		sgn direction +!
	loop
	direction @ sgn
	dup direction !
;

variable wdPos
: wrongDirection ( - cnt )
	-1 wdPos !
	direction @ 0 ( dir cnt )
	laddrRange do ( dir cnt )
		over ( d c d )
		i c@ b->s ( d c d v )
		sgn = not if
			1+ ( dir cnt+1 )
			i laddr @ line@addr - wdPos !
		then
		dup 1 > if leave then
	loop
	swap drop ( cnt )
;

: ?diffFail ( sgn v - sgn t/f )
	2dup sgn = not if ( sgn different or 0 )
		drop
		1 exit
	then
	abs 3 > ( oor? )
;

variable remPos
variable valPPos
variable valPCount
: validate ( remPos - problemPos problemCnt )
	laddr @ line@addr + ( absolutePos )
	remPos !
	-1 valPPos !
	0 valPCount !
	direction @ 0 ( sgn adj:0 )
	laddrRange do ( sgn adj )
		i c@ b->s ( sgn adj v )
		i remPos @ = if
			swap drop ( sgn v )
			cr ." adj set to " dup .
		else ( sgn adj v )
			+ ( sgn v' )
			?diffFail ( sgn fail1 )
			if ( problem )
				1 valPCount +!
				i laddr @ line@addr -
				valPPos !
			then
			0 ( sgn adj )
		then ( sgn adj )
		valPCount @ 1 > if ( terminate )
			leave
		then
	loop
	2drop
	valPPos @
	valPCount @
;

: isSafe ( laddr - t/f )
	dup .line
	laddr !
	detectDirection 0= if
		cr ." can't detect dir, so it's wrong"
		0 exit
	then
	wrongDirection
	dup 1 > if
		cr ." can't correct more than 2 dir errors"
		drop
		0 exit
	then
	0= if
		( no wrong direction )
		( good or can be corrected by dropping )
		( first element or last element )
		-1 validate ( pos cnt )
		dup 0= if
			( no problems )
			2drop
			1 exit
		then
		1 = if
			( 1 problem )
			dup ( pos pos )
			1 = swap ( p=1? pos )
			laddr @ line@cnt 1- = ( p=1? p=last? )
			or if
				( can be corrected )
				1 exit
			else
				cr ." no wrong dir, 1 problem, not first nor last"
				0 exit
			then
		else
			cr ." more than 1 problem"
			drop
			0 exit
		then
	else
		cr ." one wrong direction " wdPos ?
		wdPos @ dup ( pos pos )
		1 = swap ( p=1? pos )
		laddr @ line@cnt 1- = ( p=1? p=last? )
		or if
			( if it's first or last )
			( check dropping it )
			-1 validate ( pos cnt )
			1 = if
				dup ( pos pos )
				1 = swap ( p=1? pos )
				laddr @ line@cnt 1- = ( p=1? p=last? )
				or if
					( can be corrected )
					1 exit
				else
					cr ." ??? "
				then
			else
				drop
				cr ." first or last wrong dir, but other problem too"
			then
		then
		wdPos @
		1 > if
			( check merging with previous )
			wdPos @ 1- validate ( pos cnt )
			swap drop ( cnt )
			0= if
				1 exit
			else
				cr ." merge with prev didn't help"
			then
		then
		wdPos @
		laddr @ line@cnt 1-
		< if
			( check merging with next )
			wdPos @ validate ( pos cnt )
			swap drop ( cnt )
			0= if
				1 exit
			else
				cr ." merge with next didn't help"
			then
		then
	then
	0
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
