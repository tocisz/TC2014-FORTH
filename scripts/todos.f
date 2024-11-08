vocabulary todo.dict

( defines a new todo )
( Q: prefill it's name? )
: todo ( n [name] - )
todo.dict definitions ( can I switch it back? )
<builds
	0 c, 34 word c@ 1+ allot
does>
	dup c@ if ." [x] " else ." [ ] " then
	1+ count type cr
[ forth ]
;

( checks/unchecks given todo )
: check ( [name] - )
	' 2+ dup c@ not swap c!
;

( goes through all todos and prints them )
: todos
	todo.dict
	[ ' todo.dict 4 + @ ] literal ( stop )
	context @ @
	cr
	begin ( stop nfa )
		2dup = not while
		dup id. 9 emit
		pfa ( stop pfa )
		dup cfa execute ( stop pfa )
		lfa @ ( stop nfa' )
	repeat
	2drop
;

( todo #1 First )
( todo #2 Second item )
( todos )
( check #1 )
( todos )
