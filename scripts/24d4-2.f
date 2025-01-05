\ requires 24d4.f

: tab@  ( x y - value )
	cols @ * +
	tab @ +
	c@
;

77 constant #M
65 constant #A
83 constant #S

variable count

: ?ms ( v1 v2 - t/f )
	#M =
	swap #S =
	and
;

\ MS or SM
: ?mssm ( v1 v2 - t/f )
	2dup ( v1 v2 v1 v2 )
	?ms if
		2drop 1
	else
		swap ( v2 v1 )
		?ms
	then
;

: test ( x y - )
	2dup tab@ #A - if
		\ not A
		2drop
		exit
	then
	\ check \
	2dup ( x y x y )
	2dup ( x y x y x y )
	swap 1+ ( x y x y y x+1 )
	swap 1+ ( x y x y x+1 y+1 )
	tab@ ( x y x y v1 )
	rot 1- ( x y y v1 x-1 )
	rot 1- ( x y v1 x-1 y-1 )
	tab@ ( x y v1 v2 )
	?mssm not if
		2drop
		exit
	then
	\ check /
	2dup ( x y x y )
	swap 1+ ( x y y x+1 )
	swap 1- ( x y x+1 y-1 )
	tab@ ( x y v1 )
	rot 1- ( y v1 x-1 )
	rot 1+ ( v1 x-1 y+1 )
	tab@ ( v1 v2 )
	?mssm if
		1 count +!
		42 emit
	then
;

: cover
	0 count !
	rows @ 1- 1 do
		cols @ 1- 1 do
			i j test
		loop
	loop
	cr ." count = " count @ .
;

cover
