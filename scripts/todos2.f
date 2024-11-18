: cons ( t h -> addr )
	here >r , , r>
;

: head @ ;
: tail 2+ @ ;

variable todoL
variable todoN

: .todo
	dup c@
	dup 127 and 3 .r ." . "
	128 and if ." [x] " else ." [ ] " then
	1+ count type cr
;

: todos
	todoL @
	cls
	begin
		dup while
		dup head .todo
		tail
	repeat
	drop
;

: todo
	here
	todoN @ 1+ dup c, todoN ! ( counter )
	34 word c@ 1+ allot ( message )
	todoL @ swap cons todoL ! ( store it )
	todos
;

: check ( n - )
	todoL @
	begin
		dup while
		dup head c@ 127 and ( n todo m )
		3 pick = if
			dup head ( n todo head )
			128 toggle ( n todo )
		then
		tail
	repeat
	2drop
	todos
;
