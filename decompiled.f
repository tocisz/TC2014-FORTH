
: emit uemit @ execute 1 out +! ;
: ?terminal u?terminal @ execute ;
: cr ucr @ execute ;
: cls 12 emit ; 
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
: count dup 1+ swap c@ ;
: type ( a len - )
  ?dup
    if
        over + swap ( a+len a )
        do
            i c@ emit
        loop
    else
        drop
    then
;

: -trailing
    dup 0 ( a len len 0 )
    do ( a len )
        over over ( a len a len )
        + 1 - ( a len a+len-1 )
        c@ bl - ( a *[a+len-i]-bl )
        if leave
        else 1 - ( a len-1 )
        then
    loop
;

: <.">
  r@ ( address of next word to execute )
  ( but it contains string, so )
  count ( a len )
  dup 1+ ( a len len+1 )
  r> + >r ( fix return address )
  ( a len )
  type
;

: ."
  lit 34 ( " )
  state @
  if ( if compiling )
    compile <.">
    word c@ 1+ allot
  else ( if executing )
    word count type
  then
; immediate

: word
  blk @
  if ( read from block )
    blk @ block
  else ( read from terminal )
    tib @
  then
  >in @ + swap enclose here
  34 blanks
  >in +! over - >r r@ here c! + here 1+ r>
  cmove here
;

: expect ( a n - )
    over + over ( a a+n a )
    do ( a )
        key ( a k )
        dup lit BACKSPACE @ =
        if ( a k )
            drop ( a )
            dup i = ( a begin? )
            dup r> ( a begin? begin? i )
            2 - + ( a begin? i-2/i-1 )
            >r ( [push updated i] a begin? )
            if
                7 ( BEL )
            else
                8 ( BS )
            then
        else ( a k )
            dup lit 13 = if ( CR )
                leave
                drop bl 0 ( a ' ' 0 )
            else
                dup ( a k k )
            then
            i c! ( store key )
            0 i 1+ ! ( ensure 0 follows )
        then
        emit
    loop
    drop
;

: query
    tib @
    80 expect
    0 >in ! ( append null )
;

( null word )
: \0
    blk @
    if
        1 blk +!
        0 >in !
        blk @
        b/scr 1 - and 0=
        if ?exec r> drop then
    else
        r> drop
    then
; immediate

: blanks bl fill ;

( converts one digit of number string )
: convert
  begin
    1+ dup >r c@ base @ digit
    while ( leave loop if zero )
    swap base @ u* drop rot base @ u* d+
    dpl @ 1+
    if
        1 dpl +!
    then
    r>
  repeat
  r>
;

( converts number string )
: number ( n - ? )
  0 0 rot ( 0 0 n )
  dup 1+ c@ ( 0 0 n *[n+1] )
  lit 45 ( '-' )
  = ( 0 0 n 0/1 )
  dup >r ( sign to rstack )
  + ( 0 0 m )
  lit -1
  begin
    dpl !
    convert
    dup c@ bl - while ( while not space )
    dup c@
    lit 46 - 0 ?error ( error 0 on '.' )
    0
  repeat
  drop
  r> if dnegate then
;

: -find
  bl word
  context @ @
  <find> ( find in context dictionary )
  dup 0= if
    drop here
    latest <find> ( if not find in current )
  then
;

: <abort> abort ;

: error ( errno - )
  warning @ 0< if <abort> then
  here count type ." ? " message
  sp!
  blk @ ?dup if >in @ swap then
  quit

: id. count lit 31 and type space ;

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

( to compile next word when interpreting )
: [compile] -find 0= 0 ?error drop cfa , ;

: literal ( n - )
    state @ if ( check that it's compiling )
        compile lit ,
    then
; immediate
: dliteral
  state @ if
    swap literal literal
  then
; immediate

: ?stack
  sp@ ( sp )
  s0 @ swap ( *s0 sp )
  u< 1 ?error ( error if *s0 < sp )
  sp@ ( sp )
  here 128 + ( sp here+128 )
  u< 7 ?error ( error if sp < here+128 )
;

: interpret
  begin
    -find if
        state @ < if
            cfa , ( could optimize )
        else
            cfa execute
        then
        ?stack
    else
        here number
        dpl @ 1+ if
            dliteral
        else
            drop literal
        then
        ?stack
    then
  again
;

: immediate latest 64 toggle ;

: vocabulary
  create
  lit -24447 ,
  current @ cfa ,
  here voc-link @ ,
  voc-link !
  does> 2+ context !
;

( link? forth? )

: definitions context @ current ! ;

: (
    lit 41 ( close bracket )
    word drop
;  immediate

: quit
    0 blk !
    [ ( execute )
    begin
        rp! ( set initial rsp )
        cr
        query
        interpret
        state @ 0= if ( executing )
            <."> OK"
        then
    again
;

: abort uabort @ execute ;

: warm
    lit -31736 ( WORD1 )
    lit -1530 ( S0 )
    lit 16 ( start table lenght )
    cmove
    abort
;

: +- 0< if negate then ;
: d+- 0< if dnegate then ;
: abs dup +- ;
: dabs dup d+- ;
: min 2dup > if swap then drop ;
: max 2dup < if swap then drop ;
: m* 2dup xor >r abs swap abs u* r> d+- ;
: m/ over >r >r dabs r@ abs u/mod r> r@ xor +- swap r> +- swap ;
: * m* drop ;
: /mod >r s->d r> m/ ;
: / /mod swap drop ; 
: mod /mod drop ;
: */mod >r m* r> m/ ;
: */ */mod swap drop ;
: m/mod >r 0 r@ u/mod r> swap >r u/mod r> ;
: <line> >r c/l b/buf */mod r> b/scr * + block + c/l ;
: .line <line> -trailing type ;

: message
    warning @ if
        ?dup if
            4
            offset @
            b/scr / - .line
            space
        then
    else
        <."> MSG # " .
    then
;

: empty-buffers first limit over - erase ;
: block lit 40 mod offset @ + b/buf * first + ;
: buffer block ;
: r/w ur/w @ execute ;

: dump ( a len - )
  0 do
    cr ( a )
    dup 0 5 d.r
    space
    4 swap ( 4 a )
    over 0 do ( 4 a )
        dup c@ 3 .r 1+
    loop
    swap ( a+4 4 )
  +loop
  drop cr
;

: load blk @ >r >in @ >r 0 >in ! b/scr * blk ! interpret r> >in ! r> blk ! ;
: --> ?loading 0 >in ! b/scr blk @ over mod - blk +! ;  immediate

( literal, so it can be used when compiling, even though not immediate... )
: ' -find 0= 0 ?error drop literal ;

: forget
    current @
    context @
    - 18 ?error ( err if current != context )
    '
    dup fence @ < 15 ?error ( err if < fence )
    dup nfa dp ! ( write nfa to dp )
    lfa @
    context @ ! ( write previous word to context )
;

: back here - , ;
: begin ?comp here 1 ; immediate
: until 1 ?pairs compile 0branch back ;  immediate
: end until ;  immediate
: again 1 ?pairs compile branch back ;  immediate
: repeat >r >r again r> r> 2 - endif ;  immediate
: while if 2+ ;  immediate

: if compile 0branch here 0 , 2 ;  immediate
: else 2 ?pairs compile branch here 0 , swap 2 endif 2 ;  immediate
: endif ?comp 2 ?pairs here over - swap ! ; immediate
: then endif ;  immediate

: do compile <do> here 3 ;  immediate
: loop 3 ?pairs compile <loop> back ;  immediate
: +loop 3 ?pairs compile <+loop> back ;  immediate

: spaces
    0 max ?dup if
        0 do
            space
        loop
    then        
;

: <# pad hld ! ;
: #> drop drop hld @ pad over - ;
: sign
    rot 0< if
        lit 45 ( '-' )
        hold
    then
;
: #
    base @
    m/mod rot 9 over < if
        7 +
    then
    48 + ( '0' )
    hold
;
: #s
    do
        # over over or 0=
    until
;
: d.r >r swap over dabs <# #s sign #> r> over - spaces type ; 
: .r >r s->d r> d.r ;
: d. 0 d.r space ;
: ? @ . ;
: u. 0 d. ;
: words
    context @ @
    cr
    do
        dup pfa swap id.
        lfa @
        dup 0=
        ?terminal or
    until
    drop
    cr
;
: list
    base @
    swap
    decimal
    cr
    dup scr !
    <."> SCR # " .
    16 0 do
        cr
        i 3 .r space
        i scr @ .line
        ?terminal if leave then
    loop
    cr
    base !
;
: index
    1+ swap
    do
        cr
        i lit 3 .r space
        0 i .line
        ?terminal if leave then
    loop
    cr
;

: ;int ?csp compile X_INT [ smudge ; immediate
: .cpu <."> Z80 " ;
: 2swap rot >r rot r> ;
: 2over >r >r 2dup r> r> 2swap ;

: roll
    dup 0 > if
        dup >r
        pick r>
        0 swap
        do
            sp@ i dup + +
            dup 2- @
            swap !
        -1 +loop
    then
    drop
;
: depth s0 @ sp@ - 2 / 1- ;
: d<
    rot 2dup = if
        2drop u<
    else
        2swap 2drop >
    then
;
: 0> 0 > ;
: .s cr depth 0branch 32 sp@ 2- s0 @ 2- <do> i @ . lit -2 <+loop> -12 branch 17 <."> STACK EMPTY " ;

: code ?exec xxx sp! ;
: end-code
    current @
    context !
    ?exec
    ?csp
    smudge
;
: next
    lit 195 c,
    lit NEXT ,
; immediate

: lload
    block
    lit 0
    begin
        dup if
            dup 13 = if ( CR )
                drop c/l + c/l negate and
            else
                over !
            then
            1+
        else
            drop
        then
        key
        dup 26 = ( SUB )
    until
    drop drop
;