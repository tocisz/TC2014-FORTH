80 constant bufLen
variable buf
bufLen allot
variable numBuf
16 allot
variable len
variable tab

: readLine ( - )
	cr buf 80 expect
;

: isEmpty ( - t/f )
	buf c@ 0=
;

( parse one number, store it here, advance bufp )
: parseNum ( - n )
	32 numBuf dup c@ + 1+ c! ( ensure termination )
	numBuf number
;

: readNums ( - addr cnt )
	here
	begin
		readLine
		isEmpty not while
		buf
		begin
			32 enclose
			2dup = >r ( is it last )
			>r ( offset for the next word )
			>r ( end index )
			over + ( start )
			over r> + ( end )
			over - ( len )
			dup numBuf c! ( write len )
			numBuf 1+ swap cmove ( copy it )
			parseNum
			here [ 2 cells ] literal allot 2! ( store )
			r> + ( update addr )
		r> until
		drop
	repeat
	here over - [ 2 cells ] literal /
;

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

: perm ( i - j )
	len @ swap permutation
;

: tab@ ( i - dn )
	2u* cells tab @ + 2@
;

: tab! ( dn i - )
	2u* cells tab @ + 2!
;

: .tab
	len @ 0 do
		i tab@ d.
	loop
;

variable tmp cell allot
: cycle  ( start i - )
	dup tab@ dnegate tmp 2!
	begin
		dup perm ( start i i' )
		swap ( start i' i )
		( 2dup cr ." i=" . ."  i'=" . )
		>r 2dup ( start i' start i' R:i )
		= if ( i'==start )
			( cr ." fin tab[ " r@ . ." ] = " tmp 2@ d. )
			tmp 2@ r> tab! ( start i' )
			2drop
			exit
		else
			r> over ( start i' i i' )
			tab@ ( start i' i *i' )
			dnegate ( start i' i -*i' )
			rot ( start i' -*i' i )
			( 3 pick 3 pick 3 pick cr ." tab[ " . ." ] = " d.)
			tab! ( write to i )
		then
		( start i' )
	again
;
: unzip
	len @ 0 do
		( cr ." i=" i .)
		i tab@ 0. d< not if ( not visited )
			i dup ( start i )
			cycle
		then
	loop
	( clear -sign )
	len @ 0 do
		i tab@
		dnegate
		i tab!
	loop
;

: mid2 ( l r -- mid )
	over - 2u/ ( l [r-l]/2 )
	-4 and ( round to double cell size )
	+
;

: d> 2swap d< ;

( find position of a first value greater than val )
: binsearch2 ( l r val - addr )
	>r >r ( R: val)
	begin
		2dup swap - [ 2 cells 1+ ] literal <
		if ( 2 element range )
			over 2@ r> r> d> if
				drop
			else
				swap drop
			then
			exit
		then
		2dup mid2 ( l r mid )
		dup 2@ 2r@ d> ( l r mid *mid>val )
		if
			swap drop ( l mid )
		else
			rot drop swap ( mid r )
		then
	again
;

: 4* 2u* 2u* ;

: insertsort2 ( addr len -- )
	1 do
		dup i 2u* cells + ( l r=l+i )
		2dup dup 2@ ( l r l r *r )
		2dup >r >r ( R: *r )
		binsearch2 ( l r pos )
		swap >r ( l pos R: *r r )
		dup 2dup [ 2 cells ] literal + swap r> swap -
		( l pos pos pos+1 r-pos )
		cmove> ( l pos )
		r> r> rot 2! ( l )
	loop
	drop
;

: readData
	cr ." Enter data:"
	readNums len ! tab !
;

variable distance cell allot
: computeDistance
	cr ." Unzipping data..."
	unzip
	len @ 2u/
	cr ." Sorting (1/2)..."
	dup tab @ swap insertsort2
	cr ." Sorting (2/2)..."
	dup tab @ over 2u* cells + swap insertsort2
	cr ." Calculating distance..."
	0. distance 2!
	dup 0 do ( hl )
		dup i + tab@ ( hl tab[i+hl] )
		i tab@ ( hl tab[i+hl] tab[i] )
		dnegate d+ dabs ( hl |tab[i+hl]-tab[i]| )
		distance 2@ d+ distance 2! ( hl )
	loop
	drop
	cr ." Answer is "
	distance 2@ d. cr
;

readData
3   4
4   3
2   5
1   3
3   9
3   3

computeDistance ( 11 )
