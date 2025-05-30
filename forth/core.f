
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
                1- 3 and 1+    ( addr len addr linelen )
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
                2dup 1- 3 and 1+ ( addr len addr linelen )
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

                dup 1- 3 and 1+ ( addr len linelen )
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


