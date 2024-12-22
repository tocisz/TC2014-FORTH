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
			here [ 2 cells ] literal allot 2! ( store )
			r> + ( update addr )
		r> until
		drop
	repeat
	here over - [ 2 cells ] literal /
;

readNums
3   4
4   3
2   5
1   3
3   9
3   3

variable len
len !
variable tab
tab !

: tab@ ( i - dn )
	4 * tab @ + 2@
;

: .tab
	len @ 0 do
		i tab@ d.
	loop
;
.tab
