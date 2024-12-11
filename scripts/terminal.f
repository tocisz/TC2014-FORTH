: escapeSeq
	27 emit ( ESC )
	91 emit ( [ )
;
: gotoXY
	escapeSeq
	0 .r
	59 emit ( ; )
	0 .r
	72 emit ( H )
;
: readUntil ( addr delim - )
	swap dup 1+ rot ( addr+1 addr+1 delim )
	begin
		key
		2dup = not while
		( addr addr delim ch )
		3 pick c! ( addr addr delim )
		swap 1+ ( addr delim addr' )
		swap ( addr addr' delim )
	repeat
	2drop
	( addr addr' )
	over - 1- swap c!
;
: ?exit if r> drop exit then ;
: readXY
	escapeSeq
	54 emit ( 6 )
	110 emit ( n )
	key 27 = not ?exit ( expected ESC )
	key 91 = not ?exit ( expected [ )
	pad 6 blanks
	pad 59 readUntil ( ; )
	pad number drop
	pad 6 blanks
	pad 82 readUntil ( R )
	pad number drop
	swap
;
: readSize
	cls
	9999 9999 gotoXY
	readXY
	1 1 gotoXY
;
: asdf
	begin
		1000 0 do loop
		66 emit
	?terminal until
;
: sdfg
	begin
		1000 0 do loop
		65 emit
		?terminal if exit then
	again
;
: hr ( char length )
	0 do
		dup emit
	loop
	drop
;
: frame
	readSize
	( first line )
	over 35 swap hr
	dup 2 do
		1 i gotoXY
		35 emit ( left edge )
		over i gotoXY
		35 emit ( right edge )
	loop
	( last line )
	1 over gotoXY
	over 35 swap hr
	2 / swap 2 / 6 - swap gotoXY
	." HELLO WORLD "
	key drop
;
