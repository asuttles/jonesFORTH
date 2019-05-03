\ ----------------------------
\          UTILITY
\ ----------------------------

\ Compilation State?
: STATE? STATE @ ;

\ Print Spaces
32 CONSTANT BL
: SPACE BL EMIT ;

\ Print CRLF
13 CONSTANT CR
10 CONSTANT LF
: CRLF CR LF EMIT EMIT FLUSH ;

\ Character Literals
: ':' [ CHAR : ] LITERAL ;
: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;


\ ----------------------------
\        COMMENTS
\ ----------------------------

\ COMMENTS
: ( IMMEDIATE
	1				\ allowed nested parens by keeping track of depth
	BEGIN
		KEY			\ read next character
		DUP '(' = IF		\ open paren?
			DROP		\ drop the open paren
			1+		\ depth increases
		ELSE
			')' = IF	\ close paren?
				1-	\ depth decreases
			THEN
		THEN
	DUP 0= UNTIL			\ continue until we reach matching close paren, depth 0
	DROP				\ drop the depth counter
;

\ ----------------------------
\     PRINTING SPACE
\ ----------------------------

\ Print 'n' Spaces
: SPACES			( n -- )
	BEGIN
		DUP 0>		( while n > 0 )
	WHILE
		SPACE		( print a space )
		1-		( until we count down to 0 )
	REPEAT
	DROP
;

\ ----------------------------
\  PRINTING DECIMAL NUMBERS
\ ----------------------------

\ Print unsigned integer 'u'
: U.				( u -- )
	10 U/MOD                ( rem quot )
	?DUP IF			( if quotient <> 0 then )
		RECURSE		( print the quotient )
	THEN

				( print the remainder )
	'0' +		        ( decimal digits 0..9 )
	EMIT
;

\ Calculate width of unsigned integer
: UWIDTH			( u -- width )
	10 /			( rem quot )
	?DUP IF			( if quotient <> 0 then )
		RECURSE 1+	( return 1+recursive call )
	ELSE
		1		( return 1 )
	THEN
;

\ Print unsigned integer 'u' in a field of 'width'
: U.R				( u width -- )
	SWAP			( width u )
	DUP			( width u u )
	UWIDTH			( width u uwidth )
	ROT			( u uwidth width )
	SWAP - SPACES		( u width-uwidth )
	U.
;

\ Print signed number 'n' of 'width'
: .R				( n width -- )
	SWAP			( width n )
	DUP 0< IF
		NEGATE		( width u )
		1		( save a flag to remember that it was negative | width n 1 )
		-ROT		( 1 width u )
		SWAP		( 1 u width )
		1-		( 1 u width-1 )
	ELSE
		0		( width u 0 )
		-ROT		( 0 width u )
		SWAP		( 0 u width )
	THEN
	SWAP			( flag width u )
	DUP			( flag width u u )
	UWIDTH			( flag width u uwidth )
	ROT			( flag u uwidth width )
	SWAP -			( flag u width-uwidth )

	SPACES			( flag u )
	SWAP			( u flag )

	IF			( was it negative? print the - character )
		'-' EMIT
	THEN

	U.
;

\ Print signed number with 0 width and SPACE
: . 0 .R SPACE ;

\Re-define U. to append SPACE
: U. U. SPACE ;

\ Fetches integer at 'addr' and print
: ?				( addr -- ) @ . ;


\ ----------------------------
\  PRINTING HEX NUMBERS
\ ----------------------------

\ Print unsigned integer 'u'
: H.				( u -- )
	16 U/MOD                ( rem quot )
	?DUP IF			( if quotient <> 0 then )
		RECURSE		( print the quotient )
	THEN
	DUP 10 < IF
	            '0' + 	( decimal digits 0..9 )
	ELSE
		    10 - 'A' +  (  letter digist A..F )
        THEN
	EMIT
;


\ ----------------------------
\  PRINTING DBL LEN NUMBERS
\ ----------------------------

: DPRINT ( addr len -- )
  0 DO DUP I + C@ EMIT LOOP DROP ;

: D. ( d -- )
  D0@ DPRINT '.' EMIT SPACE ;

: UD. ( d -- )
  UD0@ DPRINT '.' EMIT ;

: D.R ( d width -- )
  -ROT              ( width LO HO )
  D0@               ( width addr len )
  ROT               ( addr len width )
  OVER - SPACES     ( addr len )
  DPRINT            ( -- )
;

HIDE DPRINT  


\ ----------------------------
\  PRINTING STACK CONTENTS
\ ----------------------------

\ Print contents of stack
: .S				( -- )
	DSP@ S0 @ 4-	        ( get stack top/bottom )
	BEGIN
		2DUP - 0<=
	WHILE
		DUP @ .	        ( print the stack element )
		SPACE
		4-		( move down )
	REPEAT
	2DROP
;


\ ----------------------------
\     PRINTING STRINGS
\ ----------------------------

\ Immediate Mode use of 'TICK' -- FIX THIS!!!
\: ' IMMEDIATE STATE? IF ' ELSE FINDWORD >CFA THEN ;

\ append byte to current word
: C,
	HERE @ C!		( store the character in the compiled image )
	1 HERE +!		( increment HERE pointer by 1 byte )
;

\ Compile a literal string
: COMPILE-STRING                ( -- )
		' LITSTRING ,	( compile LITSTRING )
		HERE @		( save initial HERE address )
		0 ,		( dummy string length )
		BEGIN
			KEY     ( addr u )
			DUP '"' <>
		WHILE
			C,	( addr )
		REPEAT
		DROP		( drop final " )
		DUP		( addr addr )
		HERE @ SWAP -	( addr len )
		4-		( correct for 'dummy' length)
		SWAP !		( -- )
		ALIGN		
;

\ Store a literal string at 'addr'
: STORE-STRING                  ( addr -- addr len )
                DUP
		BEGIN
			KEY	( addr addr u )
			DUP '"' <>      
		WHILE
			OVER C!	( save next character )
			1+	( addr addr+ -- )
		REPEAT
		DROP		( drop final " )
		OVER -		( addr len )
;

\ Define a literal string  
: S" IMMEDIATE		        ( -- addr len )  
        STATE? IF	        ( compiling? )
	        COMPILE-STRING
	ELSE		        ( immediate mode )
		HERE @	        ( temp space )
		STORE-STRING
	THEN
;

\ Echo literal string to output, until "
: ECHO-STRING 			( -- )
	BEGIN
		KEY
		DUP '"' = IF
			DROP	( drop " )
			EXIT	 
		THEN
		EMIT
	AGAIN
;

\ Print literal string
: ." IMMEDIATE		        ( -- )
	STATE? IF	        ( compiling? )
		COMPILE-STRING
		' TELL ,	          
	ELSE			( execution? )
	        ECHO-STRING
	THEN
;


\ ----------------------------
\    PRINTING DICTIONARY
\ ----------------------------

\ Print name of entry at 'addr'
: ID.	                        ( addr -- )
        NAMELEN DUP C@          ( addr -- addr len )
	BEGIN
		DUP 0>		( length > 0? )
	WHILE
		SWAP 1+		( addr len -- len addr+1 )
		DUP C@		( len addr -- len addr char )
		EMIT		( len addr char -- len addr )
		SWAP 1-		( len addr -- addr len-1 )
	REPEAT
	2DROP		        ( len addr -- )
;

\ Print all words in dictionary
: WORDS
	CRLF 
	0 LATEST @					( -- 0 addr )
	BEGIN
		?DUP					( n addr -- n addr addr )
	WHILE						( n addr addr -- n addr )
		DUP ?HIDDEN NOT IF			( NOT hidden? )
		        SWAP 1+ DUP U. ." )" SWAP	( Print word # )
			DUP ID.	CRLF			( Print name )
		THEN
		@					( Dereference pointer )
	REPEAT
	DROP CRLF
;

\ ----------------------------
\   PRINTING DECOMPILATION
\ ----------------------------

\ Put bookend addresses of WORD on stack 
: BOOKENDS                ( addr -- beg end )
    DUP >CFA 4+ SWAP OVER ( cfa addr cfa+ )
    BEGIN
        2DUP @ <>         ( cfa addr cfa+ bool )
	OVER HERE @ <>    ( cfa addr cfa+ bool bool )
        AND               ( cfa addr cfa+ bool )
    WHILE
	    4+            ( cfa addr cfa+ )
    REPEAT
    4-                    ( skip EXIT )
    NIP SWAP              ( cfa+ cfa )
;

\ Print decompiled WORD's prolog and epilog
: SEE-PROLOG
    ':' EMIT SPACE DUP ID. SPACE
    ?IMMEDIATE IF ." IMMEDIATE " THEN CRLF ;
: SEE-EPILOG ';' EMIT CRLF ;

\ Print Special FORTHWORDs: LIT and '
: SEE-LIT  4+ DUP @        . CRLF ;
: SEE-TICK 4+ DUP @ CFA> ID. CRLF ;

\ Print Special FORTHWORDs: 'LITSTRING'
: SEE-STRING-START  [ CHAR S ] LITERAL EMIT '"' EMIT SPACE ;
: SEE-STRING-STRING 4+ DUP @       ( end start+4 strLen )
                    SWAP 4+ SWAP   ( end start+8 strLen )
		    2DUP TELL      ( print the string ) ;
: SEE-STRING-END    '"' EMIT CRLF ;
: SEE-STRING-FIXUP  + ALIGNED      ( Point to next WORD )
                    4-             ( correct for next 4+ ) ;
: SEE-STRING SEE-STRING-START SEE-STRING-STRING SEE-STRING-END SEE-STRING-FIXUP ;

\ Print Special FORTHWORDs: 0BRANCH
: SEE-0BRANCH ." 0BRANCH ( " 4+ DUP @ . ." ) " CRLF ;

\ Print Special FORTHWORDs: BRANCH
: SEE-BRANCH  ." BRANCH ( "  4+ DUP @ . ." ) " CRLF ;

\ Print de-compilation of WORD
: SEE                                     ( -- )
    FINDWORD DUP BOOKENDS ROT SEE-PROLOG ( end start )
    BEGIN
	2DUP >
    WHILE
	    DUP @ CASE                    ( end start codeword )
              ' LIT       OF  SEE-LIT      ENDOF
	      ' '         OF  SEE-TICK     ENDOF
	      ' LITSTRING OF  SEE-STRING   ENDOF
              ' 0BRANCH   OF  SEE-0BRANCH  ENDOF
              ' BRANCH    OF  SEE-BRANCH   ENDOF
	      ( DEFAULT CASE )
	           DUP CFA> ID. CRLF
            ENDCASE
	    4+		                  ( end start+4 )
    REPEAT

    SEE-EPILOG
    2DROP		                  ( )
;


\ ----------------------------
\   PRINT ERR AND ABORT
\ ----------------------------
: RESTART   ( addr len -- ) TELL S0 @ DSP! QUIT ;

: TEST-FLAG ( addr len flag -- ) IF RESTART ELSE 2DROP THEN ;

: ABORT"  IMMEDIATE ( flag -- )
  COMPILE-STRING ' ROT , ' TEST-FLAG , ;

HIDE RESTART
HIDE TEST-FLAG



\ - TEST
: FILL ( addr n b -- )
  SWAP 1 DO        ( addr b )
            2DUP   ( addr b addr b )
	    SWAP   ( addr b b addr )
	    I +    ( addr b b addr+ )
	    C!     ( addr b )
   LOOP
   2DROP           ( )
;

: ERASE ( addr n -- ) 0 FILL ;
	 