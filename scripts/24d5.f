cold
\ 1. Read ordering and sequences
70 constant bufSize
variable buf
bufSize allot

: readLine ( - )
	cr buf bufSize expect
;

: isEmpty ( - t/f )
	buf c@ 0=
;

: parse2dig ( addr - n )
	32 over 2+ c! ( ensure termination )
	1- number
	drop ( single word )
;

variable orderLength
\ must be called after variable is allocated
: readPairs ( - )
	variable
	cell negate allot
	here
	begin
		readLine
		isEmpty not while
		buf parse2dig c,
		buf 3 + parse2dig c,
	repeat
	here over -
	orderLength !
;

: isDigit ( key - flag )
	dup 48 <
	swap 57 >
	or not
;

variable numOfSequences
( create 0-terminated arrays of sequences )
: readSequences ( - )
	variable
	cell negate allot
	0 numOfSequences !
	begin
		readLine
		isEmpty not while
		buf begin ( ptr )
			dup c@ isDigit while
			dup parse2dig c,
			3 + ( ptr' )
		repeat
		0 c,
		1 numOfSequences +!
		drop
	repeat
;

: readData
	readPairs
	readSequences
;

\ variable order cell negate allot
readData order sequences
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47

