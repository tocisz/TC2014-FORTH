
: emit uemit @ execute 1 out +! ;
: ?terminal u?terminal @ execute ;
: cr ucr @ execute ;
: cls lit 12 emit ; 
: not 0= ;

: : ?exec !csp current @ context ! xxx ] <;code> ... 
: ; ?comp ?csp compile ;s smudge [ ; immediate
: constant xxx smudge , <;code> ... 
: variable 0 constant <;code> ...
: user constant <;code> ...
: c/l uc/l @ ;
: first ufirst @ ; 
: limit ulimit @ ; 
: b/buf ub/buf @ ; 
: b/scr ub/scr @ ;

: here dp @ ;
: allot dp +! ; 
: , here ! 2 allot ;
: c, here c! 1 allot ;

: = - 0= ; 
: u<
  2dup xor 0< if
    drop 0< 0=
  else
    - 0<
  then
;
: > swap < ;
: pick dup + sp@ + @ ;
: space bl emit ;
: ?dup dup if dup then ;

: traverse ( a inc - a' )
  swap ( inc a )
  begin
    over + ( inc a+inc )
  127 over c@ < until ( back to begin if *[a+inc] < 127 )
  swap drop ( a+... )
; 
: latest current @ @ ;
: lfa 4 - ; ( pfa -> lfa )
: cfa 2- ; ( pfa -> cfa )
: nfa 5 - -1 traverse ; ( pfa -> nfa )
: pfa 1 traverse 5 + ; ( nfa -> pfa )
: pfa dup c@ 31 and + 5 + ;

: !csp sp@ csp ! ;
: ?error swap if error else drop then ;
: ?comp state @ 0= 17 ?error ;
: ?exec state @ 18 ?error ; 
: ?pairs - 19 ?error ;
: ?csp sp@ csp @ - 20 ?error ;
: ?loading blk @ 0= 22 ?error ; 

: compile ?comp r> dup 2+ >r @ , ;
: [ 0 state ! ; immediate
: ] 192 state ! ; 
: smudge latest 32 toggle ; 

: hex 16 base ! ;
: decimal 10 base ! ; 

: <;code> r> latest pfa cfa ! ; ( Execute following machine code )
: ;code ?csp compile <;code> [ ; immediate
: create 0 constant ; 
: does> r> latest pfa ! <;code> ...

: -find
  bl word
  context @ @
  <find> ( find in context dictionary )
  dup 0= if
    drop here
    latest <find> ( if not find in current )
  then
;

: xxx ( ? )
  -find if drop nfa id. 4 message space then 
  here ( nfa )
  dup c@ width @ min 1+ allot ( [allocate min[*dp, width]+1] nfa )
  dup 160 toggle ( [toggle 160] nfa )
  here 1 - 128 toggle ( [toggle 128 at the end] nfa )
  latest , ( [store pointer to previous word] nfa )
  current @ ! ( [store nfa] )
  here 2+ , ( cfa <- next address )
;

: code ?exec xxx sp! ;

: <abort> abort ; 