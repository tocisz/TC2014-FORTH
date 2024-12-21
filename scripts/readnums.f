80 constant bufLen
variable buf
bufLen allot
variable numBuf
16 allot

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
			drop ( drop high )
			here cell allot ! ( store low )
			r> + ( update addr )
		r> until
		drop
	repeat
	here over - 2u/
;
