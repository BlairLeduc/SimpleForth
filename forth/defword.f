
: defword
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
        4 spaces ." defword" bl emit 34 emit dup id. 34 emit 44 emit dup ?immediate if ." F_IMMED" then 44 emit dup id. cr

        >dfa                ( get the data address, ie. points after docol | end-of-word start-of-data )

        ( now we start decompiling until we hit the end of the word )
        begin                ( end start )
                2dup >
        while
                dup @                ( end start code field    )

                case
                ['] lit of                ( is it lit ? )
                        4 + dup @                ( get next word which is the integer constant )
                        4 spaces
                        ." .word LIT, "        ( print the word lit )
                        . cr                       ( and print it )
                endof
                ['] litstring of                ( is it litstring ? )
                        [ char S ] literal emit '"' emit space ( print S"<space> )
                        4 + dup @                ( get the length word )
                        swap 4 + swap                ( end start+4 length )
                        2dup tell                ( print the string )
                        '"' emit space                ( finish the string with a final quote )
                        + aligned                ( end start+4+len, aligned )
                        4 - cr                       ( because we're about to add 4 below )
                endof
                ['] 0branch of                ( is it 0branch ? )
                        4 spaces ." .word 0BRANCH, "
                        4 + dup @                ( print the offset )
                        . cr
                endof
                ['] branch of                ( is it branch ? )
                        4 spaces ." .word BRANCH, "
                        4 + dup @                ( print the offset )
                        . cr
                endof
                ['] (loop) of                ( is it (loop) ? )
                        4 spaces ." .word PAREN_LOOP, "
                        4 + dup @                ( print the offset )
                        . cr
                endof
                ['] (+loop) of                ( is it (+loop) ? )
                        4 spaces ." .word PAREN_PLUS_LOOP, "
                        4 + dup @                ( print the offset )
                        . cr
                endof
                ['] ['] of                        ( is it ['] (BRACKET_TICK) ? )
                        4 spaces ." .word BRACKET_TICK, "
                        4 + dup @                ( get the next code field    )
                        cfa>                        ( and force it to be printed as a dictionary entry )
                        id. cr
                endof
                ['] exit of                ( is it exit? )
                        ( We expect the last word to be exit, and if it is then we don't print it
                          because exit is normally implied by ;.  exit can also appear in the middle
                          of words, and then it needs to be printed. )
                        2dup                        ( end start end start )
                        4 +                        ( end start end start+4 )
                        <> if                        ( end start | we're not at the end )
                            4 spaces ." .word EXIT"
                        then
                        cr
                endof
                                        ( default case: )
                        dup                        ( in the default case we always need to dup before using )
                        cfa>                        ( look up the code field    to get the dictionary entry )
                        4 spaces ." .word "        ( print the word )
                        id. space                ( and print it )
                        cr
                endcase

                4 +                ( end start+4 )
        repeat

        ';' emit cr

        2drop                ( restore stack )
;

