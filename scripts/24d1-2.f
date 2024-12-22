( requires 24d1.f )

variable lVal cell allot
variable lCnt
variable lPtr
variable lLimit
variable rVal cell allot
variable rCnt
variable rPtr
variable rLimit
( pointers )
variable val
variable cnt
variable ptr
( not a pointer )
variable lim
: onLeft
	lVal val !
	lCnt cnt !
	lPtr ptr !
	lLimit @ lim !
;
: onRight
	rVal val !
	rCnt cnt !
	rPtr ptr !
	rLimit @ lim !
;
( handle indirection )
: val@ val @ 2@ ;
: val! val @ 2! ;
: cnt@ cnt @ @ ;
: cnt! cnt @ ! ;
: ptr@ ptr @ @ ;
: ptr! ptr @ ! ;

( init ptr and lim )
: init ( - )
	tab @
	dup lPtr !
	len @ ( tab len )
	cells dup >r + ( tab+len/2 R:len/2 )
	dup lLimit !
	dup rPtr !
	r> + ( tab+len )
	rLimit !
;

: d= ( dn dm - t/f )
	rot =
	if
		=
	else
		2drop
		0
	then
;

: advance ( - t/f )
	ptr@ lim @ < if 
		ptr@ 2@ ( val )
		2dup val!
		0 cnt!
		begin
			cnt@ 1+ cnt! ( ++cnt )
			2dup ( val val )
			ptr@ [ 2 cells ] literal +
			dup ptr! ( val val ptr )
			2@ ( val val val' )
			d= not ( val val!=val' )
			ptr@ lim @ < not or
			( val [val!=val' or ptr>=lim] )
		until
		2drop
		0
	else
		1 ( can't advance )
	then
;

variable sim cell allot
: similarity
	0. sim 2!
	init
	onLeft advance
	onRight advance
	or if exit then ( can't advance )
	begin
		lVal 2@ rVal 2@ ( lv rv )
		d= if ( values eq )
			lVal 2@
			lCnt @ rCnt @ *
			( vall valh cnt* )
			dup >r ( backup )
			u* ( high word multiplied )
			dup 0= not if ." OOR " exit then
			swap ( and shifted by 16 bits )
			sim 2@ d+ sim 2! ( add it )
			r> u* ( low word multiplied )
			sim 2@ d+ sim 2! ( add it )
			onLeft advance
			onRight advance
			or
		else lVal 2@ rVal 2@ d< if
			onLeft advance
		else
			onRight advance
		then then
	until ( can't advance )
;

similarity
sim 2@ d. ( 31 )
