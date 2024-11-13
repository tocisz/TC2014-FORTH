: cons ( t h -> addr )
	here >r , , r>
;

: head @ ;
: head@ @ @ ;
: tail 2+ @ ;

: fold ( addr acc op - acc' )
	rot ( acc op addr )
	begin
		dup >r head ( acc op head r: addr )
		swap dup >r ( acc head op r: addr op )
		execute
		r> r> ( acc' op addr )
		tail ( acc' op tail )
	dup 0= until
	2drop ( acc' )
;

variable l
0 2 cons 3 cons 4 cons l !

l @ head .
l @ tail head .
: sum 0 [ ' + cfa ] literal fold ;
: product 1  [ ' * cfa ] literal fold ;

l @ sum .
l @ product .
