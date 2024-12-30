: cons ( t h -> addr )
	here >r , , r>
;

: head @ ;
: head@ @ @ ;
: tail 2+ @ ;

( store line descriptor )
: line! ( addr cnt - laddr )
	here >r
	c, ,
	r>
;
: line@cnt ( laddr - cnt ) c@ ;
: line@addr ( laddr - addr ) 1+ @ ;

variable buf
80 allot
variable numBuf
12 allot

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

( read line of byte precission numbers )
: readNLine ( - addr cnt | 0 )
	here
	readLine
	isEmpty if
		drop 0
	else
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
			drop c, ( store 8 of 32 bits )
			r> + ( update addr )
		r> until
		drop
		here over -
	then
;

( create linked list of line descriptors )
: readNLines ( - caddr )
	0 ( list terminator )
	begin
		readNLine
		dup while
		line!
		cons
	repeat
	drop
;

variable nums

: readNums
	readNLines
	nums !
;

( hard case is a problem with first diff )
( it can be solved by removing 1st or 2nd element )
( so both cases need to be checked )
( last row is an example of this )
readNums
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
0 4 3 4 5

