: / /mod swap drop ;
: mod /mod drop ;
: '\n' 10 ;
: bl   32 ; \ bl (BLank) is a standard FORTH word for space.
: cr '\n' emit ;
: space bl emit ;
: negate 0 swap - ;
: true  -1 ;
: false 0 ;
: not   0= ;
: literal immediate
        ['] lit ,         \ compile lit
        ,               \ compile the literal itself (from the stack)
        ;
: ':'
        [               \ go into immediate mode (temporarily)
        char :          \ push the number 58 (ASCII code of colon) on the parameter stack
        ]               \ go back to compile mode
        literal         \ compile lit 58 as the definition of ':' word
;
\ here fetch the data stack pointer.
: here dp @ ;
: ';' [ char ; ] literal ;
: '(' [ char ( ] literal ;
: ')' [ char ) ] literal ;
: '"' [ char " ] literal ;
: 'A' [ char A ] literal ;
: '0' [ char 0 ] literal ;
: '-' [ char - ] literal ;
: '.' [ char . ] literal ;
: [compile] immediate
        word            \ get the next word
        (find)          \ find it in the dictionary
        >cfa            \ get its codeword
        ,               \ and compile that
;
: recurse immediate
        latest @        \ latest points to the word being compiled at the moment
        >cfa            \ get the codeword
        ,               \ compile it
;
: if immediate
        ['] 0branch ,   \ compile 0branch
        here            \ save location of the offset on the stack
        0 ,             \ compile a dummy offset
;
: then immediate
        dup
        here swap -     \ calculate the offset from the address saved on the stack
        swap !          \ store the offset in the back-filled location
;
: else immediate
        ['] branch ,    \ definite branch to just over the false-part
        here            \ save location of the offset on the stack
        0 ,             \ compile a dummy offset
        swap            \ now back-fill the original (if) offset
        dup             \ same as for then word above
        here swap -
        swap !
;
: begin immediate
        here            \ save location on the stack
;
: until immediate
        ['] 0branch ,   \ compile 0branch
        here -          \ calculate the offset from the address saved on the stack
        ,               \ compile the offset here
;
: again immediate
        ['] branch ,    \ compile branch
        here -          \ calculate the offset back
        ,               \ compile the offset here
;
: while immediate
        ['] 0branch ,   \ compile 0branch
        here            \ save location of the offset2 on the stack
        swap            \ get the original offset (from begin)
        0 ,             \ compile a dummy offset2
;
: repeat immediate
        ['] branch ,    \ compile branch
        here - ,        \ and compile it after branch
        dup
        here swap -     \ calculate the offset2
        swap !          \ and back-fill it in the original location
;
: unless immediate
        ['] not ,       \ compile not (to reverse the test)
        [compile] if    \ continue by calling the normal if
;
: ( immediate
        1               \ allowed nested parens by keeping track of depth
        begin
                key             \ read next character
                dup '(' = if    \ open paren?
                        drop            \ drop the open paren
                        1+              \ depth increases
                else
                        ')' = if        \ close paren?
                                1-              \ depth decreases
                        then
                then
        dup 0= until            \ continue until we reach matching close paren, depth 0
        drop            \ drop the depth counter
;
: nip ( x y -- y ) swap drop ;
: tuck ( x y -- y x y ) swap over ;
: pick ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
        1+              ( add one because of 'u' on the stack )
        4 *             ( multiply by the word size )
        dsp@ +          ( add to the stack pointer )
        @               ( and fetch )
;
: spaces        ( n -- )
        begin
                dup 0>          ( while n > 0 )
        while
                space           ( print a space )
                1-              ( until we count down to 0 )
        repeat
        drop
;
: decimal ( -- ) 10 base ! ;
: hex ( -- ) 16 base ! ;
: u.            ( u -- )
        base @ u/mod     ( width rem quot )
        ?dup if                 ( if quotient <> 0 then )
                recurse         ( print the quotient )
        then

        ( print the remainder )
        dup 10 < if
                '0'             ( decimal digits 0..9 )
        else
                10 -            ( hex and beyond digits A..Z )
                'A'
        then
        +
        emit
;
: .S            ( -- )
        dsp@            ( get current stack pointer )
        0 s0 @ + 4-     ( pointer to the stack element )
        begin
                over over <=    ( compare to current stack pointer )
        while
                dup @ u.        ( print the stack element )
                space
                4-              ( move down )
        repeat
        drop drop
;
: uwidth        ( u -- width )
        base @ /        ( rem quot )
        ?dup if         ( if quotient <> 0 then )
                recurse 1+      ( return 1+recursive call )
        else
                1               ( return 1 )
        then
;
: u.r           ( u width -- )
        swap            ( width u )
        dup             ( width u u )
        uwidth          ( width u uwidth )
        rot             ( u uwidth width )
        swap -          ( u width-uwidth )
        spaces
        u.
;
: .r            ( n width -- )
        swap            ( width n )
        dup 0< if
                negate          ( width u )
                1               ( save a flag to remember that it was negative | width n 1 )
                swap            ( width 1 u )
                rot             ( 1 u width )
                1-              ( 1 u width-1 )
        else
                0               ( width u 0 )
                swap            ( width 0 u )
                rot             ( 0 u width )
        then
        swap            ( flag width u )
        dup             ( flag width u u )
        uwidth          ( flag width u uwidth )
        rot             ( flag u uwidth width )
        swap -          ( flag u width-uwidth )

        spaces          ( flag u )
        swap            ( u flag )

        if                      ( was it negative? print the - character )
                '-' emit
        then

        u.
;
: . 0 .r space ;
: u. u. space ;
: ? ( addr -- ) @ . ;