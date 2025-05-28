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
        >cfa            \ get its code field   
        ,               \ and compile that
;
: recurse immediate
        latest @        \ latest points to the word being compiled at the moment
        >cfa            \ get the code field   
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
: within
        -rot            ( b c a )
        over            ( b c a c )
        <= if
                > if            ( b c -- )
                        true
                else
                        false
                then
        else
                2drop           ( b c -- )
                false
        then
;
: depth         ( -- n )
        s0 @ dsp@ -
        4-                      ( adjust because s0 was on the stack when we pushed DSP )
        4 /                     ( A cell has four bytes )
;
;
: aligned       ( addr -- addr )
        3 + 3 invert and        ( (addr+3) & ~3 )
;
: align here aligned dp ! ;
: C,
        here C!       ( store the character in the compiled image )
        1 dp +!       ( increment dp pointer by 1 byte )
;
: s" immediate          ( -- addr len )
        state @ if      ( compiling? )
                ['] litstring , ( compile litstring )
                here            ( save the address of the length word on the stack )
                0 ,             ( dummy length - we don't know what it is yet )
                begin
                        key             ( get next character of the string )
                        dup '"' <>
                while
                        C,              ( copy character )
                repeat
                drop            ( drop the double quote character at the end )
                dup             ( get the saved address of the length word )
                here swap -     ( calculate the length )
                4-              ( subtract 4 (because we measured from the start of the length word) )
                swap !          ( and back-fill the length location )
                align           ( round up to next multiple of 4 bytes for the remaining code )
        else            ( immediate mode )
                here            ( get the start address of the temporary space )
                begin
                        key
                        dup '"' <>
                while
                        over C!         ( save next character )
                        1+              ( increment address )
                repeat
                drop            ( drop the final " character )
                here -          ( calculate the length )
                here            ( push the start address )
                swap            ( addr len )
        then
;
: ." immediate          ( -- )
        state @ if      ( compiling? )
                [compile] s"    ( read the string, and compile litstring, etc. )
                ['] tell ,      ( compile the final tell )
        else
                ( In immediate mode, just read characters and print them until we get
                  to the ending double quote. )
                begin
                        key
                        dup '"' = if
                                drop    ( drop the double quote character )
                                exit    ( return from this function )
                        then
                        emit
                again
        then
;
: constant
        word            ( get the name (the name follows constant) )
        header,         ( make the dictionary entry )
        docol ,         ( append docol (the code field    field of this word) )
        ['] lit ,       ( append the code field    lit )
        ,               ( append the value on the top of the stack )
        ['] exit ,      ( append the code field    exit )
;
: allot         ( n -- addr )
        dp +!         ( adds n to here, after this the old value of here is still on the stack )
;
: cells ( n -- n ) 4 * ;
: variable
        word header,    ( make the dictionary entry (the name follows variable) )
        dodoes , 0 ,    ( append docol (the code field    field of this word) )
        1 cells allot , ( allocate one cell of memory )
;
: create
        word header,    ( make the dictionary entry (the name follows VARIABLE) )
        dodoes , 0 ,    ( append docol (the code field    field of this word) )
;

: does>
        r> latest @ >dfa !
;
: value         ( n -- )
        word header,     ( make the dictionary entry (the name follows value) )
        docol ,          ( append docol )
        ['] lit ,        ( append the code field    lit )
        ,                ( append the initial value )
        ['] exit ,       ( append the code field    exit )
;
: to immediate  ( n -- )
        word            ( get the name of the value )
        (find)          ( look it up in the dictionary )
        >dfa            ( get a pointer to the first data field (the 'lit') )
        4+              ( increment to point at the value )
        state @ if      ( compiling? )
                ['] lit ,       ( compile lit )
                ,               ( compile the address of the value )
                ['] ! ,         ( compile ! )
        else            ( immediate mode )
                !               ( update it straightaway )
        then
;
: +to immediate
        word            ( get the name of the value )
        (find)            ( look it up in the dictionary )
        >dfa            ( get a pointer to the first data field (the 'lit') )
        4+              ( increment to point at the value )
        state @ if      ( compiling? )
                ['] lit ,       ( compile lit )
                ,               ( compile the address of the value )
                ['] +! ,        ( compile +! )
        else            ( immediate mode )
                +!              ( update it straightaway )
        then
;
: id.
        4+              ( skip over the link pointer )
        dup C@          ( get the flags/length byte )
        f_lenmask and   ( mask out the flags - just want the length )

        begin
                dup 0>          ( length > 0? )
        while
                swap 1+         ( addr len -- len addr+1 )
                dup C@          ( len addr -- len addr char | get the next character)
                emit            ( len addr char -- len addr | and print it)
                swap 1-         ( len addr -- addr len-1    | subtract one from length )
        repeat
        2drop           ( len addr -- )
;
: ?hidden
        4+              ( skip over the link pointer )
        C@              ( get the flags/length byte )
        f_hidden and    ( mask the f_hidden flag and return it (as a truth value) )
;
: ?immediate
        4+              ( skip over the link pointer )
        C@              ( get the flags/length byte )
        f_immed and     ( mask the f_immed flag and return it (as a truth value) )
;
: words
        latest @        ( start at latest dictionary entry )
        begin
                ?dup            ( while link pointer is not null )
        while
                dup ?hidden not if      ( ignore hidden words )
                        dup id.         ( but if not hidden, print the word )
                        space
                then
                @               ( dereference the link pointer - go to previous word )
        repeat
        cr
;
: forget
        word (find)       ( find the word, gets the dictionary entry address )
        dup @ latest !  ( set latest to point to the previous word )
        dp !          ( and store here with the dictionary address )
;
: dump          ( addr len -- )
        base @ -rot             ( save the current base at the bottom of the stack )
        hex                     ( and switch to hexadecimal mode )

        begin
                ?dup            ( while len > 0 )
        while
                over 8 u.r      ( print the address )
                space

                ( print up to 16 words on this line )
                2dup            ( addr len addr len )
                1- 15 and 1+    ( addr len addr linelen )
                begin
                        ?dup            ( while linelen > 0 )
                while
                        swap            ( addr len linelen addr )
                        dup C@          ( addr len linelen addr byte )
                        2 .r space      ( print the byte )
                        1+ swap 1-      ( addr len linelen addr -- addr len addr+1 linelen-1 )
                repeat
                drop            ( addr len )

                ( print the ASCII equivalents )
                2dup 1- 15 and 1+ ( addr len addr linelen )
                begin
                        ?dup            ( while linelen > 0)
                while
                        swap            ( addr len linelen addr )
                        dup C@          ( addr len linelen addr byte )
                        dup 32 128 within if    ( 32 <= c < 128? )
                                emit
                        else
                                drop '.' emit
                        then
                        1+ swap 1-      ( addr len linelen addr -- addr len addr+1 linelen-1 )
                repeat
                drop            ( addr len )
                cr

                dup 1- 15 and 1+ ( addr len linelen )
                tuck            ( addr linelen len linelen )
                -               ( addr linelen len-linelen )
                >r + R>         ( addr+linelen len-linelen )
        repeat

        drop                    ( restore stack )
        base !                  ( restore saved base )
;
: case immediate
        0               ( push 0 to mark the bottom of the stack )
;
: of immediate
        ['] over ,      ( compile over )
        ['] = ,         ( compile = )
        [compile] if    ( compile if )
        ['] drop ,      ( compile drop )
;
: endof immediate
        [compile] else  ( endof is the same as else )
;
: endcase immediate
        ['] drop ,      ( compile drop )

        ( keep compiling then until we get to our zero marker )
        begin
                ?dup
        while
                [compile] then
        repeat
;
: exception-marker
        rdrop                   ( drop the original parameter stack pointer )
        0                       ( there was no exception, this is the normal return path )
;
: catch         ( xt -- exn? )
        dsp@ 4+ >r              ( save parameter stack pointer (+4 because of xt) on the return stack )
        ['] exception-marker 4+ ( push the address of the rdrop inside exception-marker ... )
        >r                      ( ... on to the return stack so it acts like a return address )
        execute                 ( execute the nested function )
;
: throw         ( n -- )
        ?dup if                 ( only act if the exception code <> 0 )
                rsp@                    ( get return stack pointer )
                begin
                        dup r0 4- <             ( RSP < r0 )
                while
                        dup @                   ( get the return stack entry )
                        ['] exception-marker 4+ = if    ( found the exception-marker on the return stack )
                                4+                      ( skip the exception-marker on the return stack )
                                rsp!                    ( restore the return stack pointer )

                                ( Restore the parameter stack. )
                                dup dup dup             ( reserve some working space so the stack for this word
                                                          doesn't coincide with the part of the stack being restored )
                                R>                      ( get the saved parameter stack pointer | n dsp )
                                4-                      ( reserve space on the stack to store n )
                                swap over               ( dsp n dsp )
                                !                       ( write n on the stack )
                                dsp! exit               ( restore the parameter stack pointer, immediately exit )
                        then
                        4+
                repeat

                ( No matching catch - print a message and restart the INTERPRETer. )
                drop

                case
                0 1- of ( abort )
                        ." aborted" cr
                endof
                        ( default case )
                        ." uncaught throw "
                        dup . cr
                endcase
                quit
        then
;

: abort         ( -- )
        0 1- throw
;
: [char] ( "<spaces>name" -- ) char ['] lit , , ; immediate
: 2over ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 ) 3 pick 3 pick ;
: cell+ ( a-addr1 -- a-addr2 ) 1 cells + ;
: chars ( n1 -- n2 ) ;
: char+ ( c-addr1 -- c-addr2 ) 1 chars + ;
: 2! ( x1 x2 a-addr -- ) swap over ! cell+ ! ;
: 2@ ( a-addr -- x1 x2 ) dup cell+ @ swap @ ;
\: move ( addr1 addr2 u -- ) cmove ;
: 2>r ( x1 x2 -- ) ( R: -- x1 x2 ) ['] swap , ['] >r , ['] >r , ; immediate
: 2r> ( -- x1 x2 ) ( R: x1 x2 -- ) ['] r> , ['] r> , ['] swap , ; immediate
: 2R@ ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
        2r> 2dup 2>r ;
: ABS   ( n -- u)
        dup 0< if negate then ;

create leave-sp 32 cells allot
leave-sp leave-sp !

: leave
        ['] unloop ,
        ['] branch ,
        leave-sp @ leave-sp - 31 cells >
        if abort then
        1 cells leave-sp +!
        here leave-sp @ !
        0 ,
        ; immediate
: resolve-leaves ( here - )
        begin
                leave-sp @ @ over >
                leave-sp @ leave-sp >  and
        while
                here leave-sp @ @ - leave-sp @ @ !
                1 cells negate leave-sp +!
        repeat
        drop
;
: do    ( -- here 0 )
        ['] (do) ,
        here 0
        ; immediate
: ?do   ( -- here 1 )
        ['] 2dup ,
        ['] <> ,
        ['] 0branch ,
        0 ,
        ['] (do) ,
        here 1
        ; immediate
: resolve-do ( here 0|1 -- here )
        if ( ?do )
                dup here - ,
                dup 2 cells - here over - swap !
        ELSE ( do )
                dup here - ,
        then ;
: loop   ( here 0|1 -- )
        ['] (loop) , 
        resolve-do
        resolve-leaves
        ; immediate
: +loop   ( here 0|1 -- )
        ['] (+loop) , 
        resolve-do
        resolve-leaves
        ; immediate
: cfa>
        latest @        ( start at latest dictionary entry )
        begin
                ?dup                ( while link pointer is not null )
        while
                2dup swap        ( cfa curr curr cfa )
                < if                ( current dictionary entry < cfa? )
                        nip                ( leave curr dictionary entry on the stack )
                        exit
                then
                @                ( follow link pointer back )
        repeat
        drop                ( restore stack )
        0                ( sorry, nothing found )
;
: see
        word (find)        ( find the dictionary entry to decompile )

        ( Now we search again, looking for the next word in the dictionary.  This gives us
          the length of the word that we will be decompiling.  (Well, mostly it does). )
        here                ( address of the end of the last compiled word )
        latest @        ( word last curr )
        begin
                2 pick                ( word last curr word )
                over                ( word last curr word curr )
                <>                ( word last curr word<>curr? )
        while                        ( word last curr )
                nip                ( word curr )
                dup @                ( word curr prev (which becomes: word last curr) )
        repeat

        drop                ( at this point, the stack is: start-of-word end-of-word )
        swap                ( end-of-word start-of-word )

        ( begin the definition with : NAME [immediate] )
        ':' emit space dup id. space
        dup ?immediate if ." immediate " then

        >dfa                ( get the data address, ie. points after docol | end-of-word start-of-data )

        ( now we start decompiling until we hit the end of the word )
        begin                ( end start )
                2dup >
        while
                dup @                ( end start code field    )

                case
                ['] lit of                ( is it lit ? )
                        4 + dup @                ( get next word which is the integer constant )
                        .                        ( and print it )
                endof
                ['] litstring of                ( is it litstring ? )
                        [ char S ] literal emit '"' emit space ( print S"<space> )
                        4 + dup @                ( get the length word )
                        swap 4 + swap                ( end start+4 length )
                        2dup tell                ( print the string )
                        '"' emit space                ( finish the string with a final quote )
                        + aligned                ( end start+4+len, aligned )
                        4 -                        ( because we're about to add 4 below )
                endof
                ['] 0branch of                ( is it 0branch ? )
                        ." 0branch ( "
                        4 + dup @                ( print the offset )
                        .
                        ." ) "
                endof
                ['] branch of                ( is it branch ? )
                        ." branch ( "
                        4 + dup @                ( print the offset )
                        .
                        ." ) "
                endof
                ['] (loop) of                ( is it (loop) ? )
                        ." (loop) ( "
                        4 + dup @                ( print the offset )
                        .
                        ." ) "
                endof
                ['] (+loop) of                ( is it (+loop) ? )
                        ." (+loop) ( "
                        4 + dup @                ( print the offset )
                        .
                        ." ) "
                endof
                ['] ['] of                        ( is it ['] (BRACKET_TICK) ? )
                        ." ['] "
                        4 + dup @                ( get the next code field    )
                        cfa>                        ( and force it to be printed as a dictionary entry )
                        id. space
                endof
                ['] exit of                ( is it exit? )
                        ( We expect the last word to be exit, and if it is then we don't print it
                          because exit is normally implied by ;.  exit can also appear in the middle
                          of words, and then it needs to be printed. )
                        2dup                        ( end start end start )
                        4 +                        ( end start end start+4 )
                        <> if                        ( end start | we're not at the end )
                                ." exit "
                        then
                endof
                                        ( default case: )
                        dup                        ( in the default case we always need to dup before using )
                        cfa>                        ( look up the code field    to get the dictionary entry )
                        id. space                ( and print it )
                endcase

                4 +                ( end start+4 )
        repeat

        ';' emit cr

        2drop                ( restore stack )
;
: :noname
        0 0 header,        ( create a word with no name - we need a dictionary header because ; expects it )
        here                ( current DP value is the address of the code field   , ie. the xt )
        docol ,                ( compile docol (the code field   ) )
        ]                ( go into compile mode )
;

: '        ( "<spaces>name" -- xt )
        word (find) >cfa
;


( Print a stack trace by walking up the return stack. )
: print-stack-trace
        rsp@                            ( start at caller of this function )
        begin
                dup r0 4- <             ( RSP < r0 )
        while
                dup @                   ( get the return stack entry )
                case
                ['] exception-marker 4+ of      ( is it the exception stack frame? )
                        ." catch ( DSP="
                        4+ dup @ u.             ( print saved stack pointer )
                        ." ) "
                endof
                                                ( default case )
                        dup
                        cfa>                    ( look up the code field    to get the dictionary entry )
                        ?dup if                 ( and print it )
                                2dup                    ( dea addr dea )
                                id.                     ( print word from dictionary entry )
                                [ char + ] literal emit
                                swap >dfa 4+ - .        ( print offset )
                        then
                endcase
                4+                      ( move up the stack )
        repeat
        drop
        cr
;


(
        WELCOME MESSAGE ----------------------------------------------------------------------

        Print the version and OK prompt.
)

: welcome
        s" TEST-MODE" (find) not if
                ." LeducForth Version " version . cr
                ." ok "
        then
;

welcome
hide welcome


