;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;
;;;                                FORTH, v0.1
;;;                            '2 stacks and ICE'
;;;
;;;                                    in
;;;                                  MASM32
;;;
;;; a port and extension of Richard Jones' Minimal FORTH compiler tutorial 
;;; http://lambda-the-ultimate.org/node/2452
;;;
;;;
;;; make with: make -k makefile
;;;
;;; (0.1) reads initialization file '.forth' upon startup
;;; Added DO-LOOP from Richard Russell
;;; Added LEAVE per SF
;;; Fixed DIVMOD per Richard Russell
;;; Fixed TRUE/FALSE to be compliant with FORTH standard (INVERT works)
;;; Added ABORT" and ?STACK to be compliant with "STARTING FORTH"
;;; Renamed RSP! and RSP@ to R! and R@ to be compliant with SF
;;; Added */ to be compliant with SF
;;; Fixed ROT/-ROT to be compliant with the standard
;;; Fixed .S to print data stack in proper order
;;; Added non-standard word: PAGE to clear console screen
;;; Added U/MOD, to be compliant with SF
;;; Added other double precision (mixed precision) math comply with SF
;;; Added non-standard word H. to print HEX values
;;; Added M+, 
;;; Added DNEGATE, DABS, DMAX, DMIN, D=, D0=, DU<, D., D.R, UD.
;;;       2R>,>2R
;;; Fixed a bug with HIDE
;;; Added M*/, UM*/ to comply with SF
;;; Added FM/MOD, /_MOD, /_, _MOD, */_MOD, and */_
;;; Added SF/REM, /-REM, /-, -REM, */-REM, */-
;;; Added 2VARIABLE,2@,2!,2CONSTANT to comply with SF Chap 8
;;; Changed VARIABLE to ALLOT memory after CODEWORD hdr to allow array creation
;;; Added FILL and ERASE to comply with SF Chap 8
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

.586

.xlist
include \masm32\include\masm32rt.inc

Main            PROTO
conInLen        PROTO :DWORD
start_file_read PROTO :DWORD
end_file_read   PROTO 
get_file_offset PROTO
set_file_offset PROTO :DWORD
FileIn          PROTO :DWORD
read_init_file  PROTO :DWORD

.list

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                  FORTH WORD DEFINITION MACROS
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

LASTWORD = 0

;;; Define Primitive FORTH Words
defCodeBlock MACRO wordName:REQ, codeName, hiddenFlag:=<FALSE>, immedFlag:=<FALSE>
   IFB <codeName>
      cname TEXTEQU  <_&wordName>            ; name of code block
      wname TEXTEQU <__&wordName>            ; name of FORTHWORD data block
   ELSE
      cname TEXTEQU  <_&codeName>
      wname TEXTEQU <__&codeName>
   ENDIF
   wname FORTHWORD {   LASTWORD,
                       &hiddenFlag,
                       &immedFlag,
                       0,
                       @SizeStr( &wordName ),
                       "&wordName",
                       OFFSET cname }
LASTWORD = wname
ENDM

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                           MANAGE RETURN STACK
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

;;; NEXT
NEXT  macro 
   mov   eax,[esi]                           ; Addr next instruction "word" 
   add   esi,4                               ; Increment IP 
   jmp   DWORD PTR [eax]                     ; Execute next "word"
ENDM  

;;; Manage Return Stack
PUSHRSP MACRO reg:REQ                        ; PUSH
   sub   ebp,4                               
   mov   [ebp],reg
ENDM  
POPRSP MACRO reg:REQ                         ; POP
   mov   reg,[ebp]
   add   ebp,4
ENDM

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                            FORTH WORD HEADER
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

   ;; Structure to Define FORTH Word Headers
FORTHWORD  struct 4
   lpPrevWord DWORD  ?                       ; Prev Word
   bHiddenF   BYTE   ?                       ; Hidden Flag
   bImmedF    BYTE   ?                       ; Immediate Flag
   bExtra     BYTE   ?                       ; DWord Pad
   bNameLen   BYTE   ?                       ; Name Length
   bName      BYTE  32 DUP (0)               ; Name (up to 32 chars)
   lpInterp   DWORD  ?                       ; Ptr to Interpreter  
FORTHWORD ENDS

   ;; FORTH Word item offsets
fwHidOffset    TEXTEQU <4>
fwImmOffset    TEXTEQU <5>
fwLenOffset    TEXTEQU <7>
fwNameOffset   TEXTEQU <8>
fwInterpOffset TEXTEQU <40>

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                   I/O BUFFERS, HEAP, Return Stack
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;; I/O Buffers
   inBUFFLEN  TEXTEQU <128>
   outBUFFLEN TEXTEQU  <32>
   wrdBUFFLEN TEXTEQU  <32>

.data 
align 4
   
   ;; I/O Buffers
   inBuffer     db inBUFFLEN  - 1 dup (?), 0 ; 128 Bytes INPUT 
   outBuffer    db outBUFFLEN - 1 dup (?), 0 ;  32 Bytes OUTPUT
   wrdBuffer    db wrdBUFFLEN - 1 dup (?), 0 ;  32 Bytes WORD name buffer

   inBuffSz     db ?                         ; Buffer Indices
   inBuffIdx    db ?
   outBuffIdx   db ?                         

align 4

   ;; Space for Defined Words
   heap         db 32768 dup(?)              ; 32k for user defined words
   ;; Return Stack
   rStack       db  8192 dup(?)              ;  8k Return Stack
   rStackTop    dd OFFSET rStackTop          ;  'Top' of the Return Stack

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                    FORTH WORD DICTIONARY HEADERS
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
align 4

   defCodeBlock <R!!>,<RSPSTORE>
   defCodeBlock <R@>,<RSPFETCH>
   defCodeBlock <RDROP>
   ;;defCodeBlock <>R>,<TOR>
   __TOR FORTHWORD {   LASTWORD,FALSE,FALSE,0,2,          ">R",OFFSET _TOR }
   LASTWORD = __TOR
   ;;defcode <R>>,<FROMR>  
   __FROMR FORTHWORD { LASTWORD,FALSE,FALSE,0,2,          "R>",OFFSET _FROMR }
   LASTWORD = __FROMR
   ;;defCodeBlock <>2R>,<TWOTOR>
   __TWOTOR FORTHWORD {   LASTWORD,FALSE,FALSE,0,3,       ">2R",OFFSET _TWOTOR }
   LASTWORD = __TWOTOR
   ;;defcode <2R>>,<TWOFROMR>  
   __TWOFROMR FORTHWORD { LASTWORD,FALSE,FALSE,0,3,       "2R>",OFFSET _TWOFROMR }
   LASTWORD = __TWOFROMR
   defCodeBlock <DROP>
   defCodeBlock <SWAP>
   defCodeBlock <DUP>
   defCodeBlock <OVER>
   defCodeBlock <2OVER>,<TWOOVER>
   defCodeBlock <ROT>
   defCodeBlock <-ROT>,<MINUSROT>
   defCodeBlock <2DROP>,<TWODROP>
   defCodeBlock <2DUP>,<TWODUP>
   defCodeBlock <2SWAP>,<TWOSWAP>
   defCodeBlock <?DUP>,<CONDDUP>
   defCodeBlock <DSP@>,<DSPFETCH>
   defCodeBlock <DSP!!>,<DSPSTORE>
   defCodeBlock <KEY>
   defCodeBlock <WORD>
   defCodeBlock <EMIT>
   defCodeBlock <FLUSH>
   defCodeBlock <TELL>
   defCodeBlock <CHAR>
   defCodeBlock <PAGE>
   defCodeBlock <D0@>,<D0FETCH>
   defCodeBlock <UD0@>,<UD0FETCH>
   defCodeBlock <NUMBER>
   defCodeBlock <1+>,<ONEPLUS>
   defCodeBlock <1->,<ONEMINUS>
   defCodeBlock <2+>,<TWOPLUS>
   defCodeBlock <2->,<TWOMINUS>
   defCodeBlock <4+>,<FOURPLUS>
   defCodeBlock <4->,<FOURMINUS>
   defCodeBlock <+>,<PLUS>
   defCodeBlock <->,<MINUS>
   defCodeBlock <*>,<MULT>
   defCodeBlock </MOD>,<DIVMOD>
   defCodeBlock <*/>,<MULTDIV>
   defCodeBlock <*/MOD>,<MULTDIVMOD>
   defCodeBlock <MIN>
   defCodeBlock <MAX>
   defCodeBlock <ABS>
   defCodeBlock <NEGATE>
   defCodeBlock <LSHIFT>
   defCodeBlock <RSHIFT>
   defCodeBlock <U/MOD>,<UDIVMOD>
   defCodeBlock <D+>,<DPLUS>
   defCodeBlock <D->,<DMINUS>
   ;;defCodeBlock <S>D>,<SIGNEXTEND>
   __SIGNEXTEND FORTHWORD { LASTWORD,FALSE,FALSE,0,3,        "S>D",OFFSET _SIGNEXTEND }
   LASTWORD = __SIGNEXTEND
   ;;defCodeBlock <U>D>,<SIGNEXTEND>
   __UNSIGNEXTEND FORTHWORD { LASTWORD,FALSE,FALSE,0,3,      "U>D",OFFSET _UNSIGNEXTEND }
   LASTWORD = __UNSIGNEXTEND
   ;;defCodeBlock <D>S>,<DOUBLETOSINGLE>
   __DOUBLETOSINGLE FORTHWORD { LASTWORD,FALSE,FALSE,0,3,    "D>S",OFFSET _DOUBLETOSINGLE }
   LASTWORD = __DOUBLETOSINGLE
   defCodeBlock <M*>,<MMULT>
   defCodeBlock <M+>,<MPLUS>
   defCodeBlock <UDM*>,<UDMMULT>
   defCodeBlock <UTM/>,<UTMDIV>
   defCodeBlock <SM/REM>,<SMDIVREM>
   defCodeBlock <UM*>,<UMMULT>
   defCodeBlock <UM/MOD>,<UMDIVMOD>
   defCodeBlock <=>,<EQUAL>
   ;;defCodeBlock <<>>,<NOTEQUAL>
   __NOTEQUAL FORTHWORD { LASTWORD,FALSE,FALSE,0,2,          "<>",OFFSET _NOTEQUAL }
   LASTWORD = __NOTEQUAL
   ;;defCodeBlock <<>,<LESSTHAN>
   __LESSTHAN FORTHWORD { LASTWORD,FALSE,FALSE,0,1,          "<",OFFSET _LESSTHAN }
   LASTWORD = __LESSTHAN
   ;;defCodeBlock <>>,<GREATERTHAN>
   __GREATERTHAN FORTHWORD { LASTWORD,FALSE,FALSE,0,1,       ">",OFFSET _GREATERTHAN }
   LASTWORD = __GREATERTHAN
   ;;defCodeBlock <<=>,<LESSTHANEQUAL>
   __LESSTHANEQUAL FORTHWORD { LASTWORD,FALSE,FALSE,0,2,     "<=",OFFSET _LESSTHANEQUAL }
   LASTWORD = __LESSTHANEQUAL
   ;;defCodeBlock <>=>,<GREATERTHANEQUAL>
   __GREATERTHANEQUAL FORTHWORD { LASTWORD,FALSE,FALSE,0,2,  ">=",OFFSET _GREATERTHANEQUAL }
   LASTWORD = __GREATERTHANEQUAL
   ;;defCodeBlock <0=>,<EQUAL0>
   __EQUAL0 FORTHWORD { LASTWORD,FALSE,FALSE,0,2,            "0=",OFFSET _EQUAL0 }
   LASTWORD = __EQUAL0
   ;;defCodeBlock <0<>>,<NOTEQUAL0>
   __NOTEQUAL0 FORTHWORD { LASTWORD,FALSE,FALSE,0,3,         "0<>",OFFSET _NOTEQUAL0 }
   LASTWORD = __NOTEQUAL0
   ;;defCodeBlock <0<>,<LESSZERO>
   __LESS0 FORTHWORD { LASTWORD,FALSE,FALSE,0,2,             "0<",OFFSET _LESS0 }
   LASTWORD = __LESS0
   ;;defCodeBlock <0>>,<GREATERZERO>
   __GREATER0 FORTHWORD { LASTWORD,FALSE,FALSE,0,2,          "0>",OFFSET _GREATER0 }
   LASTWORD = __GREATER0
   ;;defCodeBlock <0<=>,<LESSTHANEQUALZERO>
   __LESSTHANEQUAL0 FORTHWORD { LASTWORD,FALSE,FALSE,0,3,    "0<=",OFFSET _LESSTHANEQUAL0 }
   LASTWORD = __LESSTHANEQUAL0
   ;;defCodeBlock <0>=>,<GREATERTHANEQUALZERO>
   __GREATERTHANEQUAL0 FORTHWORD { LASTWORD,FALSE,FALSE,0,3, "0>=",OFFSET _GREATERTHANEQUAL0 }
   LASTWORD = __GREATERTHANEQUAL0
   ;;defCodeBlock <U<>,<ULESSTHAN>
   __ULESSTHAN FORTHWORD { LASTWORD,FALSE,FALSE,0,2,         "U<",OFFSET _ULESSTHAN }
   LASTWORD = __ULESSTHAN
   ;;defCodeBlock <DU<>,<DULESSTHAN>
   __DULESSTHAN FORTHWORD { LASTWORD,FALSE,FALSE,0,3,        "DU<",OFFSET _DULESSTHAN }
   LASTWORD = __DULESSTHAN
   defCodeBlock <AND>
   defCodeBlock <OR>
   defCodeBlock <XOR>
   defCodeBlock <INVERT>
   defCodeBlock <@>,<FETCH>
   defCodeBlock <!!>,<STORE>
   defCodeBlock <2@>,<TWOFETCH>
   defCodeBlock <2!!>,<TWOSTORE>
   defCodeBlock <C@>,<BYTEFETCH>
   defCodeBlock <C!!>,<BYTESTORE>
   defCodeBlock <C@C!!>,<BYTEFETCHSTORE>
   defCodeBlock <CMOVE>
   defCodeBlock <+!!>,<MEMORYPLUS>
   defCodeBlock <-!!>,<MEMORYMINUS>
   defCodeBlock <FIND>
   ;;defCodeBlock <>CFA>,<CFA>
   __CFA FORTHWORD {     LASTWORD,FALSE,FALSE,0,4,           ">CFA",OFFSET _CFA }
   LASTWORD = __CFA
   __INV_CFA FORTHWORD { LASTWORD,FALSE,FALSE,0,4,           "CFA>",OFFSET _INV_CFA }
   LASTWORD = __INV_CFA
   defCodeBlock <BRANCH>
   defCodeBlock <0BRANCH>,<OBRANCH>
   defCodeBlock <DO>
   defCodeBlock <+LOOP>,<PLUSLOOP>
   defCodeBlock <LOOP>
   defCodeBlock <LEAVE>
   defCodeBlock <UNLOOP>
   defCodeBlock <I>
   defCodeBlock <J>
   defCodeBlock <K>
   ;;defCodeBlock <,>,<COMMA>
   __COMMA FORTHWORD { LASTWORD,FALSE,FALSE,0,1,               ",",OFFSET _COMMA }
   LASTWORD = __COMMA
   defCodeBlock <CREATE>
   defCodeBlock <[>,<LBRAC>,,TRUE
   defCodeBlock <]>,<RBRAC>
   defCodeBlock <IMMEDIATE>,,,TRUE
   defCodeBlock <HIDDEN>
   defCodeBlock <HIDE>
   defCodeBlock <LIT>
   defCodeBlock <2LIT>,<TWOLIT>
   ;;defCodeBlock <'>
   __TICK FORTHWORD { LASTWORD,FALSE,FALSE,0,1,               "'",OFFSET _LIT }
   LASTWORD = __TICK
   defCodeBlock <LITSTRING>
   ;;defCodeBlock <;>,<SEMICOLON>
   __SEMICOLON FORTHWORD { LASTWORD,FALSE,TRUE,0,1,           ";",OFFSET _SEMICOLON }
   LASTWORD = __SEMICOLON
   defCodeBlock <:>,<COLON>
   defCodeBlock <INTERPRET>
   defCodeBlock <EXECUTE>
   defCodeBlock <BYE>
   defCodeBlock <EXIT>

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                         FORTH DERIVED WORDS
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

defWord MACRO wordName:REQ, immedFlag:=<FALSE>
   __&wordName FORTHWORD { LASTWORD,FALSE,&immedFlag,0,@SizeStr( wordName ),
                           "&wordName",OFFSET _DOCOL }
   LASTWORD = __&wordName
ENDM

DO MACRO wordName:REQ
   dd OFFSET __&wordName.lpInterp
ENDM

CC MACRO wordName:REQ
   DO LIT                       ; Equiv to: ' FOO ,
   DO &wordName
   DO COMMA
ENDM

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

   ;; QUIT
defWord <QUIT>                  ; Reset return stack & interpret words
   DO R0
   DO FETCH
   DO RSPSTORE
   DO INTERPRET                 ; INTERPRETER <-|
   DO BRANCH                    ; REPL          |
   dd (-8)                      ; ---------------

   ;; Compiler Stuff
defWord <LITERAL>,TRUE          ; Compute LITERALS at compile time
   CC LIT
   DO COMMA
   DO EXIT

;;defWord <[COMPILE]>,TRUE        ; Compile IMMEDIATE FORTH word
__FORCECOMPILE FORTHWORD {LASTWORD,FALSE,TRUE,0,9,"[COMPILE]",OFFSET _DOCOL }
   LASTWORD = __FORCECOMPILE
   DO FINDWORD
   DO CFA
   DO COMMA
   DO EXIT

defWord <RECURSE>,TRUE          ; Make 'being compiled' word recursive
   DO LATEST
   DO FETCH
   DO CFA
   DO COMMA
   DO EXIT

__NONAME FORTHWORD {LASTWORD,FALSE,FALSE,0,7,":NONAME",OFFSET _DOCOL }
   LASTWORD = __NONAME
   DO LIT
   dd 0
   DO LIT
   dd 0
   DO CREATE                                 ; CREATE 'unnamed' FORTHWORD
   DO HERE
   DO FETCH                                  ; PUSH XT
   DO LIT
   dd OFFSET _DOCOL
   DO COMMA                                  ; Compile DOCOL
   DO RBRAC                                  ; Start compilation
   DO EXIT

__COMPTICK FORTHWORD {LASTWORD,FALSE,FALSE,0,3,"[']",OFFSET _DOCOL }
   LASTWORD = __COMPTICK
   DO TICK
   DO LIT
   DO COMMA
   DO EXIT

   ;; Math
__DIVIDE FORTHWORD {LASTWORD,FALSE,FALSE,0,1,"/",OFFSET _DOCOL }
   LASTWORD = __DIVIDE
   DO DIVMOD
   DO SWAP
   DO DROP
   DO EXIT

defWord <MOD>
   DO DIVMOD
   DO DROP
   DO EXIT

defWord <NOR>
   DO OR
   DO NOT
   DO EXIT

__TWOTIMES FORTHWORD {LASTWORD,FALSE,FALSE,0,2,"2*",OFFSET _DOCOL }
   LASTWORD = __TWOTIMES
   DO LIT
   dd 1
   DO LSHIFT
   DO EXIT

__TWODIV FORTHWORD {LASTWORD,FALSE,FALSE,0,2,"2/",OFFSET _DOCOL }
   LASTWORD = __TWODIV
   DO LIT
   dd 1
   DO RSHIFT
   DO EXIT

defWord <SIGN>
   DO LESS0
   DO OBRANCH
   dd 20
   DO LIT
   dd -1
   DO BRANCH
   dd 12
   DO LIT
   dd 1
   DO EXIT

defWord <DSIGNABS>
   DO TWODUP
   DO DSIGN
   DO MINUSROT
   DO DABS
   DO EXIT

defWord <SETSIGN>
   DO LESS0
   DO OBRANCH
   dd 8
   DO NEGATE
   DO EXIT

   ;;defWord <MULTMULT>
__MULTMULT FORTHWORD {LASTWORD,FALSE,FALSE,0,2,"**",OFFSET _DOCOL }
   LASTWORD = __MULTMULT
   DO MULT
   DO MULT
   DO EXIT
   
   ;; Double Math
defWord <DNEGATE>                            ; ( D -- -D )
   DO LIT
   dd 0                                      ; ( LO HO 0     )
   DO DUP                                    ; ( LO HO 0  0  )
   DO TWOSWAP                                ; ( 0  0  LO HO )
   DO DMINUS                                 ; ( -LO -HO     )
   DO EXIT

defWord <DABS>                               ; ( D -- |D| )
   DO DUP
   DO LESS0
   DO OBRANCH
   dd 8
   DO DNEGATE
   DO EXIT

defWord <DMAX>                               ; ( D D -- D )
   DO TWOOVER                                
   DO TWOOVER                                ; ( D1 D2 D1 D2 )
   DO DMINUS                                 ; ( D1 D2 dD    )
   DO SWAP
   DO DROP                                   ; ( D1 D2 dD_HO )
   DO LESS0
   DO OBRANCH                                ; IF deltaD < 0
   dd 20
   DO TWOSWAP
   DO TWODROP                                ; D2
   DO BRANCH
   dd 8
   DO TWODROP                                ; D2
   DO EXIT

defWord <DMIN>                               ; ( D D -- D )
   DO TWOOVER                                
   DO TWOOVER                                ; ( D1 D2 D1 D2 )
   DO DMINUS                                 ; ( D1 D2 dD    )
   DO SWAP
   DO DROP                                   ; ( D1 D2 dD_HO )
   DO LESS0
   DO OBRANCH                                ; IF deltaD < 0
   dd 16
   DO TWODROP                                ; D1
   DO BRANCH
   dd 12
   DO TWOSWAP
   DO TWODROP                                ; D2
   DO EXIT

defWord <DSIGN>
   DO TWOLIT
   dq 0
   DO DLESSTHAN
   DO OBRANCH
   dd 20
   DO LIT
   dd -1
   DO BRANCH
   dd 12
   DO LIT
   dd 1
   DO EXIT

   ;;defWord <UM*/>                          ; ( d s s -- d )
__UMMULTDIV FORTHWORD {LASTWORD,FALSE,FALSE,0,4,"UM*/",OFFSET _DOCOL }
   LASTWORD = __UMMULTDIV
   DO TOR                                    ; ( d s )
   DO UDMMULT                                ; ( t )
   DO FROMR                                  ; ( t s )
   DO UTMDIV                                 ; ( d )
   DO EXIT

   ;;defWord <M*/>                           ; ( d s s -- d )
__MMULTDIV FORTHWORD {LASTWORD,FALSE,FALSE,0,3,"M*/",OFFSET _DOCOL }
   LASTWORD = __MMULTDIV
   DO TOR                                    ; ( d s ) 
   DO TOR                                    ; ( d )
   DO DSIGNABS                               ; ( sign |d| )
   DO RSPFETCH                               ; ( sign |d| s )
   DO SIGN                                   ; ( sign |d| sign )
   DO MINUSROT                               ; ( sign sign |d| )
   DO FROMR                                  ; ( sign sign |d| s )
   DO ABS                                    ; ( sign sign |d| |s| )
   DO UDMMULT                                ; ( sign sign |t| )
   DO RSPFETCH                               ; ( sign sign |t| s )
   DO ABS                                    ; ( sign sign |t| |s| )
   DO UTMDIV                                 ; ( sign sign |d| )
   DO FROMR                                  ; ( sign sign |d| s )
   DO MINUSROT                               ; ( sign sign s |d| )
   DO TWOTOR                                 ; ( sign sign s )
   DO SIGN                                   ; ( sign sign sing )
   DO MULTMULT                               ; ( sign )
   DO TWOFROMR                               ; ( sign |d| )
   DO ROT                                    ; ( |d| sign )
   DO SETSIGN                                ; ( d )
   DO EXIT

defWord <DSETSIGN>
   DO LESS0
   DO OBRANCH
   dd 8
   DO DNEGATE
   DO EXIT

   ;; FM/MOD and derivatives...
__FMDIVMOD FORTHWORD {LASTWORD,FALSE,FALSE,0,6,"FM/MOD",OFFSET _DOCOL }
   LASTWORD = __FMDIVMOD
   DO DUP                 ;  ( -- d1	n1 n1                              )  
   DO TOR                 ;  ( -- d1	n1		R: -- n1	   )  
   DO LESS0               ;  ( -- d1	flag		R: -- n1	   )  
   DO OBRANCH             
   dd 8                   
   DO DNEGATE             
   DO SIGNEXTEND          ;  ( -- d1l d1hl d1hh		R: -- n1   )  
   DO RSPFETCH            ;  ( -- d1l d1hl d1hh n1	R: -- n1   )  
   DO ABS                 ;  ( -- d1l d1hl d1hh _n1_	R: -- n1   )  
   DO AND                 ;  ( -- d1l d1hl intermed	R: -- n1   )  
   DO PLUS                ;  ( -- d1l intermed		R: -- n1   )  
   DO RSPFETCH            ;  ( -- d1l intermed n1	R: -- n1   )  
   DO ABS                 ;  ( -- d1l intermed _n1_	R: -- n1   )  
   DO UMDIVMOD            ;  ( -- n2' n3		R: -- n1   )  
   DO SWAP                ;  ( -- n3 n2'		R: -- n1   )  
   DO FROMR               ;  ( -- n3 n2' n1		R: --	   )  
   DO LESS0               ;  ( -- n3 n2' flag			   )       
   DO OBRANCH           
   dd 8                 
   DO NEGATE            
   DO SWAP                ;  ( -- n2 n3                            )  
   DO EXIT

   ;; : /_MOD ( n1 n2 -- n3 n4) >R S>D R> FM/MOD ;
__DIVFLOORMOD FORTHWORD {LASTWORD,FALSE,FALSE,0,5,"/_MOD",OFFSET _DOCOL }
   LASTWORD = __DIVFLOORMOD
   DO TOR
   DO SIGNEXTEND
   DO FROMR
   DO FMDIVMOD
   DO EXIT

   ;; : /_  ( n1 n2 -- n3)  /_MOD SWAP DROP ;
__DIVFLOOR FORTHWORD {LASTWORD,FALSE,FALSE,0,2,"/_",OFFSET _DOCOL }
   LASTWORD = __DIVFLOOR
   DO DIVFLOORMOD
   DO SWAP
   DO DROP
   DO EXIT

   ;; : _MOD ( n1 n2 -- n3)   /_MOD DROP ;
__FLOORMOD FORTHWORD {LASTWORD,FALSE,FALSE,0,4,"_MOD",OFFSET _DOCOL }
   LASTWORD = __FLOORMOD
   DO DIVFLOORMOD
   DO DROP
   DO EXIT

   ;; : */_MOD ( n1 n2 n3 -- n4 n5)  >R M* R> FM/MOD ;
__MULTDIVFLOORMOD FORTHWORD {LASTWORD,FALSE,FALSE,0,6,"*/_MOD",OFFSET _DOCOL }
   LASTWORD = __MULTDIVFLOORMOD
   DO TOR
   DO MMULT
   DO FROMR
   DO FMDIVMOD
   DO EXIT

   ;; : */_  ( n1 n2 n3 -- n4 )   */_MOD SWAP DROP ;
__MULTDIVFLOOR FORTHWORD {LASTWORD,FALSE,FALSE,0,3,"*/_",OFFSET _DOCOL }
   LASTWORD = __MULTDIVFLOOR
   DO MULTDIVFLOORMOD
   DO SWAP
   DO DROP
   DO EXIT

   ;; Derivatives of SM/REM

   ;; : /-REM  ( n1 n2 -- n3 n4 )  >R  S>D  R> SM/REM ;
__SDIVREM FORTHWORD {LASTWORD,FALSE,FALSE,0,5,"/-REM",OFFSET _DOCOL }
   LASTWORD = __SDIVREM
   DO TOR
   DO SIGNEXTEND
   DO FROMR
   DO SDIVREM
   DO EXIT

   ;; : /-  (  n1 n2 -- n3 )  /-REM SWAP DROP ;
__SDIV FORTHWORD {LASTWORD,FALSE,FALSE,0,2,"/-",OFFSET _DOCOL }
   LASTWORD = __SDIV
   DO SDIVREM
   DO SWAP
   DO DROP
   DO EXIT

   ;; : -REM  ( n1 n2 -- n3 )  /-REM DROP ;
__SREM FORTHWORD {LASTWORD,FALSE,FALSE,0,4,"-REM",OFFSET _DOCOL }
   LASTWORD = __SREM
   DO SDIVREM
   DO DROP
   DO EXIT

   ;; : */-REM  (  n1 n2 n3 -- n4 n5 )  >R  M*  R> SM/REM ;
__SMULTDIVREM FORTHWORD {LASTWORD,FALSE,FALSE,0,6,"*/-REM",OFFSET _DOCOL }
   LASTWORD = __SMULTDIVREM
   DO TOR
   DO MMULT
   DO FROMR
   DO SMDIVREM
   DO EXIT

   ;; : */-  ( n1 n2 n3 -- n4 )  */-REM SWAP DROP ;
__SMULTDIV FORTHWORD {LASTWORD,FALSE,FALSE,0,5,"/-REM",OFFSET _DOCOL }
   LASTWORD = __SMULTDIV
   DO SMULTDIVREM
   DO SWAP
   DO DROP
   DO EXIT

   ;; Double Comparisons
__DEQUAL FORTHWORD {LASTWORD,FALSE,FALSE,0,2,"D=",OFFSET _DOCOL }
   LASTWORD = __DEQUAL                       ; ( D1 D2 -- bool )
   DO ROT                                    ; ( LO1 LO2 HO2 HO1 )
   DO EQUAL                                  ; ( LO1 LO2 bool )
   DO MINUSROT                               ; ( bool LO1 LO2 )
   DO EQUAL                                  ; ( bool bool )
   DO AND                                    ; ( bool )
   DO EXIT

__DZEROEQUAL FORTHWORD {LASTWORD,FALSE,FALSE,0,3,"D0=",OFFSET _DOCOL }
   LASTWORD = __DZEROEQUAL                   ; ( D1 -- bool )
   DO EQUAL0                                 ; ( LO bool )
   DO SWAP                                   ; ( bool LO )
   DO EQUAL0                                 ; ( bool bool )
   DO AND                                    ; ( bool )
   DO EXIT

__DLESSTHAN FORTHWORD {LASTWORD,FALSE,FALSE,0,2,"D<",OFFSET _DOCOL }
   LASTWORD = __DLESSTHAN                    ; ( D1 D2 -- bool )
   DO DMINUS                                 ; ( dLO dHO ), d=Delta
   DO SWAP                                   ; ( dHO dLO )
   DO DROP                                   ; ( dHO )
   DO LESS0                                  ; ( bool )
   DO OBRANCH
   dd 20                                     ; IF TRUE
   DO LIT
   dd -1                                     ; -> TRUE
   DO BRANCH
   dd 12
   DO LIT                                    ; ELSE 
   dd 0                                      ; -> FALSE
   DO EXIT

   ;; Booleans
defWord <TRUE>
   DO LIT
   dd -1
   DO EXIT

defWord <FALSE>
   DO LIT
   dd 0
   DO EXIT

defWord <NOT>
   DO EQUAL0
   DO EXIT


   ;; IF Control Structures
   ;;
   ;; <test> IF     <true code> [ELSE <false code>] THEN ...
   ;; <test> UNLESS <true code> [ELSE <false code>] THEN ...
defWord <IF>,TRUE
   CC OBRANCH                   ; Compile 0BRANCH
   DO HERE
   DO FETCH                     ; Location of BRANCH-TO ADDR
   DO LIT
   dd 0                         
   DO COMMA                     ; 'dummy' offset to FALSE part
   DO EXIT

defWord <THEN>,TRUE
   DO DUP                       ; DUP BRANCH-TO ADDR
   DO HERE
   DO FETCH                     ; Get ADDR FALSE part
   DO SWAP
   DO MINUS                     ; Calc offset between them
   DO SWAP
   DO STORE                     ; Store OFFSET after 0BRANCH
   DO EXIT                      ; (or ELSE, if defined)

defWord <ELSE>,TRUE
   CC BRANCH                    ; Compile BRANCH over FALSE code                   
   DO HERE
   DO FETCH                     ; Location of BRANCH-TO ADDR
   DO LIT
   dd 0
   DO COMMA                     ; 'dummy' offset over FALSE part
   DO SWAP                      ; 'IF' OFFSET on top of stack
   DO DUP
   DO HERE
   DO FETCH
   DO SWAP
   DO MINUS                     ; OFFSET between IF and ELSE
   DO SWAP
   DO STORE                     ; Store OFFSET after 0BRANCH
   DO EXIT

defWord <UNLESS>,TRUE           ; IF with reversed conditional
   CC EQUAL0                    ; 'NOT' the conditional
   DO IF
   DO EXIT

   ;; Loop Control Structures
   ;; 
   ;; DO-WHILE: BEGIN    <code>        <conditional> UNTIL
   ;; WHILE(1): BEGIN    <code>                      AGAIN
   ;; WHILE:    BEGIN <conditional> WHILE <code>     REPEAT
defWord <BEGIN>,TRUE         ; Where to loop back to
   DO HERE
   DO FETCH
   DO EXIT

defWord <UNTIL>,TRUE            ; Conditional Loop back to BEGIN
   CC OBRANCH                   ; Compile 0BRANCH
   DO HERE
   DO FETCH
   DO MINUS                     ; OFFSET from BEGIN
   DO COMMA
   DO EXIT

defWord <AGAIN>,TRUE            ; Unconditional loop back to BEGIN
   CC BRANCH                    ; Compile BRANCH
   DO HERE
   DO FETCH
   DO MINUS                     ; OFFSET from BEGIN
   DO COMMA
   DO EXIT

defWord <WHILE>,TRUE            ; Sets off looped code
   CC OBRANCH                   ; Compile 0BRANCH
   DO HERE
   DO FETCH
   DO LIT
   dd 0                         ; dummy OFFSET to end of LOOP
   DO COMMA                     ; for a FALSE (ie skip loop)
   DO EXIT

defWord <REPEAT>,TRUE           ; Sets off end of WHILE loop
   CC BRANCH                    ; Compile BRANCH
   DO SWAP                      ; Put ADDR of BEGIN on top 
   DO HERE
   DO FETCH
   DO MINUS
   DO COMMA                     ; Calculate OFFSET TO BEGIN
   DO DUP
   DO HERE
   DO FETCH
   DO SWAP
   DO MINUS                     ; Calculate OFFSET TO WHILE
   DO SWAP
   DO STORE                     ; Store OFFSET after WHILE's 0BRANCH
   DO EXIT

   ;; CASE Switch (compile only)
   ;;
   ;; test1 OF ... ENDOF
   ;; test2 OF ... ENDOF
   ;; testn OF ... ENDOF
   ;; ...                       ( default case )
   ;; ENDCASE			
defWord <CASE>,TRUE             ; Mark bottom of stack
   DO LIT
   dd 0
   DO EXIT

defWord <OF>,TRUE
   CC OVER
   CC EQUAL
   DO IF
   CC DROP
   DO EXIT

defWord <ENDOF>,TRUE
   DO ELSE
   DO EXIT

defWord <ENDCASE>,TRUE          ; Keep compiling THEN until stack marker
   CC DROP
   DO CONDDUP
   DO OBRANCH
   dd 16
   DO THEN
   DO BRANCH
   dd -20
   DO EXIT

   ;; Fancy Stack Juggling
defWord <NIP>                   ; ( x y -- y )
   DO SWAP
   DO DROP
   DO EXIT

defWord <TUCK>                  ; ( x y -- y x y )
   DO DUP
   DO MINUSROT
   DO EXIT

defWord <PICK>                  ; ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
   DO ONEPLUS
   DO LIT
   dd 4
   DO MULT
   DO DSPFETCH
   DO PLUS                      ; Add OFFSET to stack pointer
   DO FETCH
   DO EXIT

defWord <DEPTH>                 ; ( -- n )
   DO S0
   DO FETCH
   DO DSPFETCH
   DO MINUS
   DO FOURMINUS                 ; adjust for S0 on stack
   DO LIT
   dd 4
   DO DIVIDE
   DO EXIT

defWord <?STACK>                ; Stack Underflow
   DO S0
   DO FETCH
   DO DSPFETCH
   DO MINUS
   DO LESSTHANEQUAL0
   DO EXIT

   ;; Memory
defWord <FORGET>                ; ( -- )
   DO WORD
   DO FIND                      ; Find ADDR of word to forget
   DO DUP
   DO FETCH                     ; Find prev word for LATEST
   DO LATEST
   DO STORE
   DO HERE                      ; Set HERE to THIS ADDR
   DO STORE
   DO EXIT

defWord <ALLOT>                 ; ( n -- ), n = #bytes
   DO HERE                      ; ( n [here] )
   DO MEMORYPLUS                ; [here] += n
   DO EXIT

defWord <CELLS>                 ; ( n -- n )
   DO LIT                       
   dd 4
   DO MULT                      ; CELL = 4 bytes
   DO EXIT

defWord <ALIGNED>               ; ( addr -- addr )
   DO LIT                       ; addr+3 & ~3
   dd 3
   DO PLUS
   DO LIT
   dd 3
   DO INVERT
   DO AND
   DO EXIT

defWord <ALIGN>                 ; ( -- )
   DO HERE                      ; Align HERE
   DO FETCH
   DO ALIGNED
   DO HERE
   DO STORE
   DO EXIT

defWord <FILL>                  ; ( addr n byte -- )
   DO SWAP                      ; ( addr byte n )
   DO LIT
   dd 1                         ; ( addr byte n 1 )
   DO DO                        ; ( addr byte )
   DO TWODUP                    ; ( addr byte addr byte )
   DO SWAP                      ; ( addr byte byte addr )
   DO I                         ; ( addr byte byte addr I )
   DO PLUS                      ; ( addr byte byte addr+ )
   DO BYTESTORE                 ; ( addr byte )
   DO LOOP
   DO TWODROP                   ; ( -- )
   DO EXIT

defWord <ERASE>                 ; ( addr n -- )
   DO LIT
   dd 0
   DO FILL
   DO EXIT

   ;; Dictionary
defWord <NAMELEN>               ; ( addr -- addr+fwLenOffset )
   DO LIT
   dd fwLenOffset
   DO PLUS
   DO EXIT

__ISHIDDEN FORTHWORD {LASTWORD,FALSE,FALSE,0,7,"?HIDDEN",OFFSET _DOCOL }
   LASTWORD = __ISHIDDEN
   DO LIT
   dd fwHidOffset
   DO PLUS
   DO BYTEFETCH
   DO EXIT

__ISIMMEDIATE FORTHWORD {LASTWORD,FALSE,FALSE,0,10,"?IMMEDIATE",OFFSET _DOCOL }
   LASTWORD = __ISIMMEDIATE
   DO LIT
   dd fwImmOffset
   DO PLUS
   DO BYTEFETCH
   DO EXIT

   ;; Constants/Variables
defWord <CONSTANT>
   DO COLON                     ; Define Codeword
   CC LIT                       ; Compile 'LIT'
   DO COMMA                     ; Compile constant
   DO SEMICOLON                 ; Add to dictionary
   DO EXIT

defWord <2CONSTANT>
   DO COLON                     ; Define Codeword
   DO TOR
   CC LIT                       ; Compile 'LIT'
   DO COMMA                     ; Compile constant LO Word
   DO FROMR
   CC LIT                       ; Compile 'LIT'
   DO COMMA                     ; Compile constant HO Word
   DO SEMICOLON                 ; Add to dictionary
   DO EXIT

                                ; VARIABLES do not align memory!!!
                                ; Programs should align after using 
defWord <VARIABLE>
   DO HERE
   DO FETCH
   DO LIT                       ; Reserve Space CELL-sized variable
   dd 56                        ; (HDR:44) + (LIT:4) + (ADDR:4) + (EXIT:4)
   DO PLUS                      ; ADDR OFFSET
   DO CONSTANT                  ; Create constant pointer to var
   DO LIT
   dd 4
   DO ALLOT                     ; Create space for variable 
   DO EXIT

defWord <2VARIABLE>
   DO VARIABLE                  ; Create an additional CELL
   DO LIT
   dd 4
   DO ALLOT
   DO EXIT

   ;; Constant Ratios for Integer Math
defWord <PI>                    ; Approx PI, Error = 8.5E-8
   DO LIT
   dd 355
   DO LIT
   dd 113
   DO EXIT

defWord <OVERPI>
   

defWord <ROOT2>                 ; Approx SQR(2), Error = 1.5E-9
   DO LIT
   dd 19601
   DO LIT
   dd 13860
   DO EXIT

defWord <ROOT3>                 ; Approx SQR(3), Error = 1.1E-9
   DO LIT
   dd 18817
   DO LIT
   dd 10864
   DO EXIT

defWord <E>                     ; Approx e, Error = 5.5e-9
   DO LIT
   dd 28667
   DO LIT
   dd 10546
   DO EXIT

defWord <LOG2>                  ; Approx log(2), Error = 1.1E-8
   DO LIT
   dd 2040
   DO LIT
   dd 11103
   DO EXIT

defWord <LN2>                   ; Approx ln(2), Error = 1.0E-7
   DO LIT
   dd 485
   DO LIT
   dd 11464
   DO EXIT

   ;; Additional Logic Test
defWord <WITHIN>                ; a <= c < b ( c a b -- bool )
   DO MINUSROT                       ; ( b c a )
   DO OVER                      ; ( b c a c )
   DO LESSTHANEQUAL             ; IF a < c
   DO OBRANCH                   ; 
   dd 32                        ; 
   DO GREATERTHAN               ; THEN IF b > c
   DO OBRANCH                   ; 
   dd 12                        ; 
   DO TRUE                      ;      THEN TRUE
   DO EXIT                      ; 
   DO FALSE                     ;      ELSE FALSE
   DO EXIT                      ; 
   DO TWODROP                   ; ELSE FALSE
   DO FALSE                     ; 
   DO EXIT

   ;; Handy Shorthand
defWord <FINDWORD>
   DO WORD
   DO FIND  
   DO EXIT

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                              DOCOL INTERPRETER
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
   .code 

_DOCOL:
   PUSHRSP esi                               ; Push ESI on return stack
   add   eax,4                               ; eax points to first data word
   mov   esi,eax                              
   NEXT

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                              BUILT-IN VARIABLES
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

defVar MACRO varName:REQ, hiddenFlag:=<FALSE>, immedFlag:=<FALSE>, initial:=<0>
.data
   defCodeBlock &varName,, &hiddenFlag, &immedFlag
   &varName dd &initial
.code
_&varName:
   push OFFSET &varName
   NEXT
ENDM

   ;; STATE: Is the interpreter executing code (0) or compiling a word (non-zero)?
   executeState TEXTEQU <0>
   compileState TEXTEQU <1>

   defVar <STATE>,,,executeState

   ;; IOMODE: Defines whether input is from a file or console
   fileMODE     TEXTEQU <0>
   consoleMODE  TEXTEQU <1>

   defVar <IOMODE>,,,consoleMODE

   ;; HERE: Points to next free byte of memory.  
   ;;       When compiling, compiled words go here.
   defVar <HERE>,,,OFFSET heap

   ;; S0: Stores the address of the top of the parameter stack.
   ;;     This should be set during program initialization
   defVar <S0>

   ;; R0: The address of the top of the return stack.
   defVar <R0>,,,rStackTop

   ;; LATEST: Points to the latest (most recently defined) word in the dictionary.
   ;;         NOTE: Must be final defined word, if initialized with "LATEST"
.data
align 4
   defCodeBlock <LATEST>
   LATEST dd __LATEST.lpPrevWord

.code
_LATEST:
   push OFFSET LATEST
   NEXT

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                RETURN STACK
;;;
;;; NOTE: See I, J, and K in DO/LOOP Section
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

_RSPSTORE:	
   pop   [ebp]
   NEXT  

_RSPFETCH:
   push  [ebp]
   NEXT  

_RDROP:
   add  ebp,4                                ;; pop return stack and throw away
   NEXT  

   ;; Single Precision Integers (32-bit)

_TOR:
   pop     eax                               ;; pop parameter stack into eax
   PUSHRSP eax                               ;; push it on to the return stack
   NEXT  
   
_FROMR:
   POPRSP eax                                ;; pop return stack on to eax
   push   eax                                ;; and push on to parameter stack
   NEXT  
   
   ;; Double Precision Integers (64-bit)

_TWOTOR:
   pop   eax                                 ; HO
   pop   ebx                                 ; LO
   PUSHRSP ebx
   PUSHRSP eax
   NEXT

_TWOFROMR:
   POPRSP eax                                ; HO
   POPRSP ebx                                ; LO
   push   ebx
   push   eax
   NEXT

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                      DATA (PARAMETER) STACK MANAGEMENT
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

_DROP:
   pop   eax                                 ;; drop top of stack
   NEXT  

_SWAP:
   pop   eax                                 ; swap top 2 stack elements
   pop   ebx
   push  eax
   push  ebx
   NEXT  

_DUP:
   mov   eax,[esp]		             ; duplicate top of stack
   push  eax
   NEXT  
   
_OVER:                                       ; ( a b -- a b a )
   mov   eax,[esp+4]	                     ; get the second element of stack
   push  eax		                     ; and push it on top
   NEXT  

_UNDER:                                      ; ( a b -- a a b ) 
   pop   eax
   mov   ebx,[esp]
   push  ebx
   push  eax
   NEXT

_TWOOVER:
   mov   eax,[esp+8]
   mov   ebx,[esp+12]
   push  ebx
   push  eax
   NEXT
   
_ROT:
   pop   eax
   pop   ebx
   pop   ecx
   push  ebx
   push  eax
   push  ecx
   NEXT  
   
_MINUSROT:
   pop   eax
   pop   ebx
   pop   ecx
   push  eax
   push  ecx
   push  ebx
   NEXT  
   
_TWODROP:
   pop   eax                                 ; drop top 2 elements of stack
   pop   eax
   NEXT  
   
_TWODUP:
   mov   eax,[esp]                           ; dup top 2 elements of stack
   mov   ebx,[esp+4]
   push  ebx
   push  eax
   NEXT  
   
_TWOSWAP:
   pop   eax                                 ; swap top 2 pairs of elements of stack
   pop   ebx
   pop   ecx
   pop   edx
   push  ebx
   push  eax
   push  edx
   push  ecx
   NEXT  
   
_CONDDUP:
   mov   eax,[esp]                           ; duplicate top of stack if non-zero
   test  eax,eax
   jz    @F
   push  eax
@@: 
   NEXT

_DSPFETCH:
   mov   eax,esp
   push  eax
   NEXT  
   
_DSPSTORE:
   pop   esp
   NEXT  

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                           BUFFERED CONSOLE INPUT
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

_KEY:                                        ; Return input KEY in EAX 
   call  _KEYREAD
   push  eax		
   NEXT  

_KEYREAD:
   xor   eax,eax
   mov   al,inBuffSz

   .while ( al == inBuffIdx )                ; Exhausted the input buffer?

      call _KEYREAD_READ

   .endw 

   ;; Return KEY in eax, if NOT comment
   xor   ebx,ebx
   xor   eax,eax
   mov   bl,inBuffIdx
   mov   al,BYTE PTR inBuffer[ebx]

   .if ( al == 92 )             ; Skip Comments '\' to EOL
      mov inBuffSz,bl
      jmp _KEYREAD              ; Force new KEYREAD
   .endif

   inc   inBuffIdx
   ret

_KEYREAD_INPUT_PROMPT:
   .if   ( STATE == executeState )          
      print chr$(32,32,62,32,32)                ; Input prompt ' > '
   .else 
      print chr$(32,32,58,32,32)                ;              ' : ' 
   .endif 
   ret   

_KEYREAD_READ:
   .if ( IOMODE == consoleMODE )
      ;; Read from Console
      call _KEYREAD_INPUT_PROMPT
      invoke StdIn,    OFFSET inBuffer, inBUFFLEN
      invoke conInLen, OFFSET inBuffer
      mov inBuffSz,al
      mov inBuffIdx,0
   .else
      ;; Read from File
      invoke FileIn,    OFFSET inBuffer
      .if eax == 0                           ; No input? 
         jmp _KEYREAD_READ
      .else
         inc eax
         mov inBuffSz,al
      .endif
   .endif

   mov   inBuffIdx,0
   ret

_FLUSH_INPUT:                                ; Flush input buffer, if desired
   mov   al,inBuffSz
   mov   inBuffIdx,al
   ret   

_WORD:
   call  _WORDREAD
   push  OFFSET wrdBuffer                    ; push base address
   push  ecx		                     ; push length
   NEXT

_WORDREAD:
   call  _KEYREAD                            ; get next 'key', returned in eax

   ;; Skip Leading Whitespace and Tab Char
   .if al <= 32
      jmp  _WORDREAD
   .endif

   ;; Read/Store the Word
   xor   ecx,ecx                             ; Initialize counter
   .while  eax > 32
      mov   BYTE PTR wrdBuffer[ecx],al
      inc   ecx
      .break .if ecx == wrdBUFFLEN           ; MAX wordlength 
      push  ecx
      call  _KEYREAD
      pop  ecx
   .endw

   mov   BYTE PTR wrdBuffer[ecx],0           ; NULL terminate word
   ret

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                          BUFFERED CONSOLE OUTPUT
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

_EMIT:                                       
   pop   eax                                 ; Write char to buffer output
   call  _WRITE_BYTE
   NEXT

_WRITE_BYTE:           
   xor   ebx,ebx
   mov   bl,outBuffIdx
   mov   BYTE PTR outBuffer[ebx],al
   inc   ebx
   .if ( ebx == outBUFFLEN )
      call _FLUSH_OUTPUT
   .else
      mov outBuffIdx,bl
   .endif
   ret

_FLUSH:
   xor ebx,ebx
   mov bl,outBuffIdx
   call _FLUSH_OUTPUT
   NEXT
_FLUSH_OUTPUT:                               ; FLUSH output buffer 
   .if ebx > 0
      mov BYTE PTR outBuffer[ebx],0
      invoke StdOut, OFFSET outBuffer
      mov   outBuffIdx,0
   .endif
   ret

_TELL:
   pop   ecx		                     ; length of string
   pop   edx		                     ; address of string
   add   ecx,edx

   .while ( edx < ecx )                      ; EMIT each byte of string
      xor eax,eax
      mov al,[edx]   
      push ecx
      push edx
      call _WRITE_BYTE
      pop edx
      pop ecx
      inc edx
   .endw

   push  esi
   xor   ebx,ebx
   mov   bl,outBuffIdx
   call  _FLUSH_OUTPUT
   pop   esi
   NEXT

_CHAR:
   call  _WORDREAD
   xor   eax,eax
   mov   al,wrdBuffer                        ; Get first char of next WORD
   push  eax                                 ; Put it on data stack
   NEXT  

_PAGE:
   cls                                       ; Clear console MACRO
   NEXT

   ;; DOUBLE LENGTH NUMBER - MAX NUM: 18,446,744,073,709,551,616 (20 digist)
   ;; BUFFERED CONSOLE OUTPUT

   .data
   align 4

   dblBuffer   db 24 dup(?)
   dblSignFmt  db "%I64d",0
   udblSignFmt db "%I64u",0

   .code

_D0FETCH:                                    ; Get Dbl output buffer
   pop   edx                                 ; HO
   pop   eax                                 ; LO
   mov   dblBuffer[0],0
   invoke wsprintf, OFFSET dblBuffer, OFFSET dblSignFmt, edx::eax
   push  OFFSET dblBuffer
   push  eax
   NEXT

_UD0FETCH:                                   ; Get Dbl output buffer
   pop   edx                                 ; HO
   pop   eax                                 ; LO
   mov   dblBuffer[0],0
   invoke wsprintf, OFFSET dblBuffer, OFFSET udblSignFmt, edx::eax
   push  OFFSET dblBuffer
   push  eax
   NEXT

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                 FILE I/O
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

;;; TBA   

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                               PARSE NUMBERS
;;; Uses Base 10 only at this time.
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

isNumber     TEXTEQU <bl !>= 030h && bl !<= 039h>
isDelimiter  TEXTEQU <bl !>= 02Ch && bl !<= 02Fh>
isNumSymbol  TEXTEQU <bl !>= 02Ch && bl !<= 039h>

DOUBLE_DIG   TEXTEQU <TRUE>
NEG_SIGN     TEXTEQU <0>
POS_SIGN     TEXTEQU <1>

_NUMBER:
   pop   ecx		                     ; length of string
   pop   edi		                     ; start address of string
   call  _NUMBER_PARSE

   push  eax		                     ; Signed 32-bit Integer
   .if ebx == DOUBLE_DIG                       
      push edx                               ; Signed 64-bit Integer?
   .endif
   push  ecx		                     ; num of unparsed chars (0=no error)
   NEXT  

_NUMBER_PARSE:
   xor   eax,eax                             ; Accumulator
   xor   ebx,ebx                             ; Next char to parse
   xor   edx,edx                             ; HO for dbl length numbers

   pushd FALSE                               ; Flag for Double-Length Nums 

   mov   bl,[edi]		             ; bl = first char in string
   inc   edi

   ;;    Check if first character is '-'.
   .if   ( bl == '-' ) 

      .if (ecx == 1)                         ; Single length word?
         ret
      .endif

      dec ecx
      mov bl,[edi]                           ; Read next char 
      inc edi
      pushd NEG_SIGN                         ; Save 'neg sign' on stack
   .else
      pushd POS_SIGN                         ; Save 'pos sign' on stack
   .endif

   ;; Process the number characters
   .while isNumSymbol

      dec   ecx 

      .if isDelimiter                        ; Double Number 
         mov DWORD PTR [esp+4],1

      .else

         sub   ebx,30h                       ; Convert char to integer 
         imul  eax,10                           
         add   eax,ebx
         adc   edx,0
         
      .endif

      .break .if ecx == 0 
      mov  bl,[edi]
      inc  edi
   .endw

   pop   ebx                                 ; sign the integer
   .if ebx == NEG_SIGN
      neg eax
      sbb ebx,edx
      mov edx,ebx
   .endif

   pop   ebx                                 ; DOUBLE FLAG
   ret

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                           BASIC ARITHMETIC
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл


   ;; SINGLE PRECISION (32-bit) *SIGNED* INTEGERS

_ONEPLUS:
   inc  SDWORD PTR [esp]                      ; increment top of stack
   NEXT  
   
_ONEMINUS:
   dec  SDWORD PTR [esp]                      ; decrement top of stack
   NEXT  

_TWOPLUS:
   add  SDWORD PTR [esp],2                    ; add 2 to top of stack
   NEXT  
   
_TWOMINUS:
   sub  SDWORD PTR [esp],2                    ; subtract 2 from top of stack
   NEXT  
   
_FOURPLUS:
   add  SDWORD PTR [esp],4                    ; add 4 to top of stack
   NEXT  
   
_FOURMINUS:
   sub  SDWORD PTR [esp],4                    ; subtract 4 from top of stack
   NEXT  
   
_PLUS:
   pop   eax                                 ; get top of stack
   add   [esp],eax                           ; and add it to next word on stack
   NEXT  
   
_MINUS:
   pop   eax                                 ; get top of stack
   sub   [esp],eax	                     ; and subtract it from next word on stack
   NEXT  
   
_MULT:
   pop   eax
   pop   ebx
   imul  ebx
   push  eax                                 ; ignore overflow
   NEXT  

_DIVMOD:
   pop   ebx
   pop   eax
   cdq                                       ; FIX to JONESFORTH! (by RTR)
   idiv  ebx
   push  edx                                 ; push remainder
   push  eax                                 ; push quotient
   NEXT  

_MULTDIV:
   pop   ecx                                 ; Divisor
   pop   ebx                                 ; Multiplicand 1
   pop   eax                                 ; Multiplicand 2
   imul  ebx                                 ; Result in EDX:EAX 
   idiv  ecx                                 ; EDX:EAX / ECX
   push  eax                                 ; eax = quotient, edx = remainder
   NEXT

_MULTDIVMOD:
   pop   ecx                                 ; Divisor
   pop   ebx                                 ; Multiplicand 1
   pop   eax                                 ; Multiplicand 2
   imul  ebx                                 ; Result in EDX:EAX 
   idiv  ecx                                 ; EDX:EAX / ECX
   push  edx                                 ; edx = remainder
   push  eax                                 ; eax = quotient
   NEXT

_MIN:
   pop   eax
   pop   ebx
   .if ( SDWORD PTR eax < SDWORD PTR ebx )
      push eax
   .else
      push ebx
   .endif
   NEXT

_MAX:
   pop   eax
   pop   ebx
   .if ( SDWORD PTR eax > SDWORD PTR ebx )
      push eax
   .else
      push ebx
   .endif
   NEXT
   
_ABS:
   pop   eax
   .if ( SDWORD PTR eax < 0 )
      not   eax
      inc   eax
   .endif 
   push  eax
   NEXT

_NEGATE:
   pop   eax
   not   eax
   inc   eax
   push  eax
   NEXT

   ;; BIT TWIDDLING (32-bit) 

_LSHIFT:
   pop   ecx                                 ; Number bits to shift
   pop   eax                                 ; Value to shift
   sal   eax,cl
   push  eax
   NEXT

_RSHIFT:
   pop   ecx                                 ; Number bits to shift
   pop   eax                                 ; Value to shift
   sar   eax,cl
   push  eax
   NEXT

   ;; SINGLE PRECISION (32-bit) *UNSIGNED* INTEGERS

_UDIVMOD:                                    ; U/DIVMOD
   pop   ebx
   pop   eax
   xor   edx,edx
   div   ebx                                 ; unsigned division
   push  edx                                 ; push remainder
   push  eax                                 ; push quotient
   NEXT  

   ;; DOUBLE PRECISION (64-bit) INTEGERS
   ;; note: FORTH is Big-Endian

_DPLUS:                                      ; D+
   pop   edx                                 ; Word 1 H.O.
   pop   eax                                 ; Word 1 L.O.
   add   SDWORD PTR [esp+4],eax              ; L.O.
   adc   SDWORD PTR [esp],edx                ; H.O.
   NEXT

_DMINUS:                                     ; D-
   pop   edx                                 ; Word 1 H.O.
   pop   eax                                 ; Word 1 L.O.
   sub   SDWORD PTR [esp+4],eax               ; L.O.
   sbb   SDWORD PTR [esp],edx                 ; H.O.
   NEXT

   ;; SINGLE TO DOUBLE PRECISION SIGNED INTEGERS
   ;; 32-bit inputs, 64-bit outputs

_SIGNEXTEND:                                 ; S>D
   pop   eax                                 ; 32-bit word
   cdq                                       ; 64-bit word
   push  eax                                 ; Push L.O.
   push  edx                                 ; Push H.O.
   NEXT

_MMULT:                                      ; M*
   pop   eax                                 ; multiplicand 1
   pop   ebx                                 ; multiplicand 2
   imul  ebx                                 ; signed multiply
   push  eax                                 ; L.O.
   push  edx                                 ; H.O.
   NEXT

   ;; MIXED PRECISION SIGNED INTEGERS
_MPLUS:                                      ; ( d n -- dsum )
   pop   eax                                 ; 32-bit word
   add   SDWORD PTR [esp+4],eax               ; Add L.O. Words
   adc   SDWORD PTR [esp],0                   ; Add H.O. Words
   NEXT

_UDMMULT:                                    ; ( ud us -- ut ), ut = 96-bits
   pop   ebx                                 ; Single-len multiplicand 1
   pop   ecx                                 ; HO multiplicand 2
   pop   eax                                 ; LO multiplicand 2
   ;; Mixed Precision Multiplication
   mul   ebx
   push  eax                                 ; ( LO )
   push  edx                                 ; ( LO Carry )
   mov   eax,ecx
   mul   ebx                                 ; -> LO 
   pop   ebx                                 ; ( LO ); ebx = carry
   add   eax,ebx                             ; -> LO2
   push  eax                                 ; ( LO LO2 )
   adc   edx,0                               ; -> HO
   PUSH  edx                                 ; ( LO LO2 HO )
   NEXT

_UTMDIV:                                     ; ( t s -- d )
   ;; Mixed Precision Division
   ;;  - Triple Length Dividend (96 bits) / Single Length Divisor (32 bits)
   ;;  - Save Double Length quotient (64 bits)
   ;; ASSUME divisor != 0
   pop   ebx                                 ; Divisor
   pop   eax
   cdq                                       ; EDX::EAX = Dividend
   div   ebx
   pop   eax                                 ; Destroy HO Word
   div   ebx
   mov   ecx,eax                             ; Store HO 64-bit quotient (TEMP)
   pop   eax                                 ; Get LO of 96-bit dividend
   div   ebx
   push  eax                                 ; Save LO 64-bit quotient
   push  ecx                                 ; Save HO 64-bit quotient
   NEXT                                      ; Throw away remainder in edx

   ;; SM/REM
_SMDIVREM:                                   ; ( d s -- s s )
   pop   ecx		                     ; s1
   pop   edx		                     ; dh
   pop   eax		                     ; dl
   idiv  ecx
   ;; ASSUME quotient smaller than 32-bits!
   push  edx		                     ; remainder
   push  eax		                     ; symmetric quotient
   NEXT	

   ;; SINGLE TO DOUBLE PRECISION UNSIGNED INTEGERS
   ;; 32-bit inputs, 64-bit outputs

_UMMULT:                                     ; UM* 
   pop   ebx                                 ; ( u1 u2 -- ud )                                
   pop   eax
   mul   ebx                                 ; unsigned multiply
   push  eax                                 ; L.O. Word
   push  edx                                 ; H.O. Word
   NEXT

_UMDIVMOD:                                   ; UM/MOD
   pop   ebx                                 ; Single Len  Divisor
   pop   edx                                 ; H.O. 32-bit Dividend
   pop   eax                                 ; L.O. 32-bit Dividend
   div   ebx                                 ; unsigned division
   push  edx                                 ; push remainder
   push  eax                                 ; push quotient
   NEXT  

_UNSIGNEXTEND:                               ; U>D
   pushd 0                                   ; Push H.O.
   NEXT

   ;; DOUBLE TO SINGLE PRECISION SIGNED INTEGERS
   ;; 64-bit input, 32-bit output

_DOUBLETOSINGLE:
   pop   eax                                 ; Remove H.O.   
   and   eax,80000000H                       ; Get sign bit
   or    [esp],eax                           ; Set sign bit
   NEXT


;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                         COMPARISON OPERATORS
;;;
;;; NOTE: ANS compliant; returns all bits set
;;;       Jones Forth is not ANS compliant on representation for True/False
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл


   ;; SINGLE PRECISION (32-bit) *SIGNED* INTEGERS

_EQUAL:
   pop   eax                                ;; Equal?
   pop   ebx

   .if SDWORD PTR ebx == SDWORD PTR eax 
      pushd -1
   .else 
      pushd FALSE
   .endif 
   NEXT  
   
_NOTEQUAL:
   pop   eax                                ;; Not Equal?
   pop   ebx
   .if SDWORD PTR ebx == SDWORD PTR eax
      pushd FALSE
   .else
      pushd -1
   .endif
   NEXT  
   
_LESSTHAN:
   pop   eax                                 ;; Less Than?
   pop   ebx
   .if SDWORD PTR ebx < SDWORD PTR eax
      pushd -1
   .else
      pushd FALSE
   .endif
   NEXT  
   
_GREATERTHAN:
   pop   eax                                 ;; Greater Than?
   pop   ebx
   .if SDWORD PTR ebx > SDWORD PTR eax
      pushd -1
   .else
      pushd FALSE
   .endif
   NEXT  
   
_LESSTHANEQUAL:
   pop   eax                                 ;; Less Than or Equal To?
   pop   ebx
   .if SDWORD PTR ebx <= SDWORD PTR eax
      pushd -1
   .else
      pushd FALSE
   .endif
   NEXT  
   
_GREATERTHANEQUAL:
   pop   eax                                 ;; Greater Than or Equal To?
   pop   ebx
   .if SDWORD PTR ebx >= SDWORD PTR eax
      pushd -1
   .else
      pushd FALSE
   .endif
   NEXT  
   
_EQUAL0:
   pop   eax                                 ;; Equal to Zero?
   .if eax == 0
      pushd -1
   .else
      pushd FALSE
   .endif
   NEXT  
   
_NOTEQUAL0:
   pop   eax                                 ;; Not Equal to Zero?
   .if eax != 0 
      pushd -1
   .else
      pushd FALSE
   .endif
   NEXT  
   
_LESS0:
   pop   eax                                 ; Less Than Zero?
   .if SDWORD PTR eax < 0                    ; SIGNED comparison
      pushd -1
   .else
      pushd FALSE
   .endif
   NEXT  
   
_GREATER0:
   pop   eax                                 ;; Greater Than Zero?
   .if SDWORD PTR eax > 0
      pushd -1
   .else
      pushd FALSE
   .endif
   NEXT  
   
_LESSTHANEQUAL0:
   pop   eax                                 ;; Less Than Or Equal to Zero?
   .if SDWORD PTR eax <= 0
      pushd -1
   .else
      pushd FALSE
   .endif
   NEXT  
   
_GREATERTHANEQUAL0:
   pop   eax                                 ;; Greater Than Or Equal to Zero?
   .if SDWORD PTR eax >= 0
      pushd -1
   .else
      pushd FALSE
   .endif
   NEXT  

   ;; SINGLE PRECISION (32-bit) *UNSIGNED* INTEGERS

_ULESSTHAN:                                  ; U<
   pop   eax                                 
   pop   ebx
   .if ebx < eax                             ; unsigned less than
      pushd -1
   .else
      pushd FALSE
   .endif
   NEXT  
   
   ;; DOUBLE PRECISION (64-bit) *SIGNED* INTEGERS

                                             ; DU<
_DULESSTHAN:                                 ; ( LO1 HO1 LO2 HO2 )
   pop   eax                                 ; HO2
   pop   ecx                                 ; LO2
   pop   ebx                                 ; HO1
   pop   edx                                 ; LO1

   .if ebx < eax                             
      pushd -1

   .elseif ebx == eax
      .if edx < ecx                         
         pushd -1
      .else
         pushd 0
      .endif

   .else
      pushd 0
   .endif
   NEXT

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                            BITWISE LOGICALS
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

_AND:
   pop   eax                                ;; Bitwise AND
   and   [esp],eax
   NEXT  
   
_OR:
   pop   eax                                ;; Bitwise OR
   or    [esp],eax
   NEXT  
   
_XOR:
   pop   eax                                ;; Bitwise XOR
   xor   [esp],eax
   NEXT  
   
_INVERT:
   not   DWORD PTR [esp]                     ;; Bitwise NOT
   NEXT  


;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                             Memory Management
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл


;;; DWord Sized Fetch and Store
_FETCH:
   pop   ebx                                 ;; address to FETCH
   mov   eax,[ebx]                           ;; fetch it
   push  eax                                 ;; push value onto stack
   NEXT  

_STORE:
   pop   ebx                                 ;; address to STORE
   pop   eax                                 ;; data to store there
   mov   [ebx],eax                           ;; store it
   NEXT  
   
;;; QWord Sized Fetch and Store
_TWOFETCH:
   pop   ebx                                 ; ADDR to FETCH
   mov   eax,[ebx]                           ; FETCH LO Word
   push  eax
   mov   eax,[ebx+4]                         ; FETCH HO Word
   push  eax
   NEXT

_TWOSTORE:
   pop   ebx                                 ; ADDR to STORE
   pop   edx                                 ; HO Word
   pop   eax                                 ; LO Word
   mov   [ebx],eax
   add   [ebx+4],edx
   NEXT

;;; Byte Sized (Fetch, Store, Fetch/Store, Block Fetch/Store)
_BYTEFETCH:
   pop   ebx                                 ;; address to FETCH
   xor   eax,eax
   mov   al,[ebx]                            ;; fetch byte
   push  eax                                 ;; push value onto stack
   NEXT  

_BYTESTORE:
   pop   ebx                                 ;; address to STORE
   pop   eax                                 ;; data to store there
   mov   [ebx],al                            ;; store byte
   NEXT  
   
_BYTEFETCHSTORE:
   pop   edi                                 ;; STORE Address
   pop   ebx                                 ;; FETCH Address
   mov   al,[ebx]                            ;; Fetch byte
   inc   ebx
   mov   [edi],al                            ;; Store byte
   inc   edi
   push  ebx                                 ;; Source Address
   push  edi                                 ;; Destination Address
   NEXT

_CMOVE:
   pop   ecx                                 ;; BLOCK Length
   pop   edi                                 ;; STORE Address
   pop   ebx                                 ;; FETCH Address (do not clobber esi)
   add   ecx,ebx                             ;; Last byte to copy
@@:
   mov   al,[ebx]                            ;; Fetch byte
   inc   ebx
   mov   [edi],al                            ;; Store byte
   inc   edi
   cmp   ecx,ebx                             ;; Last byte copied?
   jne   @b
   NEXT

;;; Arithmetic in Memory
_MEMORYPLUS:
   pop   ebx                                 ;; address
   pop   eax                                 ;; the amount to add
   add   [ebx],eax                           ;; add it
   NEXT  
   
_MEMORYMINUS:
   pop   ebx                                 ;; address
   pop   eax                                 ;; the amount to subtract
   sub   [ebx],eax                           ;; sub it
   NEXT  

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                 DICTIONARY
;;; FIND: Find dictionary pointer
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

_FIND:
   pop   ecx		                     ; ecx = length
   pop   edi		                     ; edi = address
   call  _FINDWORD
   push  eax		                     ; eax = addr dict entry (or NULL)
   NEXT  
   
   ;; Searching through linked list dictionary for this word
_FINDWORD:
   mov   eax,LATEST	                     ; 'latest' word in the dict

_FINDWORD_LOOP:
   .if eax == 0                              ; Word doesn't exist in dict?
      ret   
   .endif

   xor ebx,ebx
   mov bl,(FORTHWORD PTR [eax]).bNameLen
   .if bl != cl                              ; Word is not right length? 
      mov eax,[eax]
      jmp _FINDWORD_LOOP
   .endif

   mov bl,(FORTHWORD PTR [eax]).bHiddenF
   .if bl == TRUE                            ; Word is hidden?
      mov eax,[eax]
      jmp _FINDWORD_LOOP
   .endif

   ;; Compare characters in the word name
   push  eax  
   push  ecx
   add   eax,fwNameOffset                    ; eax points to word name
   .repeat
      dec ecx
      mov bl,[eax+ecx]
      mov dl,[edi+ecx]
      .if bl != dl                           ; Chars do NOT match?
         pop ecx
         pop eax
         mov eax,[eax]
         jmp _FINDWORD_LOOP
      .endif
   .until ecx == 0

   ;; The strings match - return
   pop   ecx
   pop   eax
   ret

   ;; Calc Code Field Address, OFFSET from FORTH Word header
_CFA:
   pop   eax
   add   eax,fwInterpOffset
   push  eax
   NEXT

_INV_CFA:
   pop   eax
   sub   eax,fwInterpOffset
   push  eax
   NEXT

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                     BRANCH
;;;
;;; Used to make control structures (FOR, WHILE, etc)
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

_BRANCH:
   add   esi,[esi]                           ; Add OFFSET to IP
   NEXT  
   
_OBRANCH:
   pop   eax
   .if ( eax == 0 )		             ; Test for TRUE/FALSE
      add   esi,[esi]                        ;    Add OFFSET to IP
   .else
      add   esi,4                            ; ELSE, continue...
   .endif
   NEXT  

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                  DO-LOOPS
;;;                             per Richard Russell
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

_DO:
   pop   ecx                                 ; index 
   pop   edx                                 ; limit
   PUSHRSP edx                               ; limit
   PUSHRSP ecx                               ; index
   PUSHRSP esi                               ; Return Addr
   NEXT  

_PLUSLOOP:
   pop   eax
   jmp   _LOOP_DO

_LOOP:
   mov   eax,1                               ; default step
_LOOP_DO:
   add   eax,[ebp+4]                         ; new index
   .if (SDWORD PTR eax == SDWORD PTR [ebp+8]); end of loop?
      jmp _LEAVE
   .endif
   mov   [ebp+4],eax                         ; save new index
   mov   esi,[ebp]                           ; Load hidden Ret Addr
   NEXT

_UNLOOP:
_LEAVE:
   add   ebp,12
   NEXT

   ;; Inner Loop Indices
_I:
   pushd [ebp+4]                             ; Inner index
   NEXT

_J:
   pushd [ebp+16]                            ; Outer index
   NEXT

_K:
   pushd [ebp+28]                            ; Outer Outer index
   NEXT

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                   CREATE
;;;                                    COMMA
;;;                        (graphic from Jones' tutorial)
;;;
;;; CREATE: Create FORTH word headers
;;; COMMA: Add an address HERE
;;;
;;;		                                    
;;;    ptr to prev word                             +-- After, HERE points here,
;;;	   ^					    |   where interpreter will 
;;;	   |					    V      start appending
;;;	+---------+---+---+---+---+---+---+-----+---+
;;;	| LINK    | # | ? | ? | ? | ? | ? | ... | 0 |
;;;	+---------+---+---+---+---+---+---+-----+---+
;;;        ^           len                         pad
;;;        |
;;;        LATEST
;;;
;;; where ? = char
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

   ;; Append a 32-bit integer (ADDRESS) at HERE
_COMMA:
   pop   eax                                 ; Integer to store
   call  _COMMA_DO
   NEXT  
_COMMA_DO:
   mov   ebx,HERE
   mov   [ebx],eax
   add   ebx,4
   mov   HERE,ebx
   ret

   ;; Create a Code/Data Field Header
_CREATE:
   pop   ecx                                 ; ecx = length
   pop   ebx                                 ; ebx = address of name
   call   _CREATE_DO
   NEXT
_CREATE_DO:
   push  HERE
   mov   eax,[LATEST]                        ; Link to LATEST word
   call  _COMMA_DO
   pop   LATEST                              ; Update LATEST

   mov   eax,ecx                             ; FORTH word name length
   shl   eax,24                              ; 3 Bytes = 0; 4th Byte = strLen
   call  _COMMA_DO

   .if   ecx > 0
      invoke szCopy, OFFSET wrdBuffer, [HERE] ; Copy FORTH word name
   .endif 
   add   HERE,32
   ret

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                  LITERALS
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

   ;; <LIT> 
_LIT:
   mov   eax,[esi]                           ; Next instruction is data
   add   esi,4                               ; Skip next instruction
   push  eax                                 ; Push data on stack
   NEXT  

_TWOLIT:
   mov   eax,[esi]                           ; HO
   mov   ebx,[esi+4]                         ; LO
   add   esi,8                               ; Skip 2 cells
   push  ebx                                 ; LO
   push  eax                                 ; HO
   NEXT

_LITSTRING:
   mov   eax,[esi]                           ; Get len of string
   add   esi,4
   push  esi                                 ; Addr of String
   push  eax                                 ; Push String Len
   add   esi,eax                             ; Skip past string
   add   esi,3                               ; DWord boundary
   mov   eax,3
   not   eax
   and   esi,eax
   NEXT

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                 CHANGE MODE
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

   ;;	Word	     Action		Effect
   ;;	[	     STATE := 0	Switch to immediate mode.
   ;;	]	     STATE := 1	Switch to compile mode.
_LBRAC:
   mov   STATE,executeState
   NEXT  
_RBRAC:
   mov   STATE,compileState
   NEXT  

   ;; Make LATEST word IMMEDIATE (Toggle)
_IMMEDIATE:
   mov   edi,LATEST                          ; LATEST word
   mov  (FORTHWORD PTR [edi]).bImmedF,TRUE
   NEXT  

   ;; Make LATEST word (un)HIDDEN - Toggle
_HIDDEN:
   pop   eax
   call  _HIDDEN_DO
   NEXT
_HIDDEN_DO:                                  ; Toggle Hidden Flag
   xor   (FORTHWORD PTR [eax]).bHiddenF,TRUE
   ;;mov   ebx,(FORTHWORD PTR [eax]).bHiddenF
   ;;xor   ebx,TRUE
   ;;mov   (FORTHWORD PTR [eax]).bHiddenF,ebx
   ret
   
   ;; Make next FORTH word (un)HIDDEN - Toggle
_HIDE:
   call  _WORDREAD                           ; Read next FORTH word 
   mov   edi,OFFSET wrdBuffer
   call  _FINDWORD                           ; Find WORD header 

   .if ( eax == 0 )
      jmp _PARSE_ERROR
   .endif
   
   call _HIDDEN_DO
   NEXT

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                    COMPILE
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

_COLON:
   call  _WORDREAD                           ; New Word
   mov   ebx,OFFSET wrdBuffer                ; ebx=wrdBuffer,ecx=counter
   call  _CREATE_DO                          ; Create Header
   mov   eax,OFFSET _DOCOL                   ; Codeword
   call  _COMMA_DO                           ; Write Interpreter Addr
   mov   eax,LATEST
   call  _HIDDEN_DO
   jmp   _RBRAC                              ; COMPILE Mode

_SEMICOLON:
   mov   eax,OFFSET __EXIT.lpInterp          ; Append EXIT
   call  _COMMA_DO
   mov   eax,LATEST                          ; Toggle unHidden
   call  _HIDDEN_DO
   jmp   _LBRAC                              ; IMMEDIATE Mode

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                INTERPRETER
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

   notFOUND     TEXTEQU <0>

.data
   errmsg db "PARSE ERROR: Cannot interpret the following > ", 0

.code
   align 4
_INTERPRET:
   call  _WORDREAD                           ; ECX = Word length
   mov   edi,OFFSET wrdBuffer                ; Word Stored in wrdBuffer

   ;; Is it in the dictionary?
   call  _FINDWORD
   .if ( eax == notFOUND )                   ; NOT in dictionary?
      jmp _NOT_IN_DICT
   .endif

   ;; Word *IS* in Dictionary
   mov   bl,(FORTHWORD PTR [eax]).bImmedF
   add   eax,fwInterpOffset

   .if  bl == TRUE                           ; Word is Immediate?
      jmp DWORD PTR [eax]
   .endif

   .if   ( STATE == executeState )           ; Execute Mode?
      jmp DWORD PTR [eax]
   .endif 
                                             ; Compile Mode
   call  _COMMA_DO                           ; Compile It in New Word
   NEXT

   ;; Word *IS NOT* in Dictionary
_NOT_IN_DICT:

   call  _NUMBER_PARSE                       ; Is it a number?

   .if   ( ecx != 0 )                        ; Parse Error?
      jmp _PARSE_ERROR
   .endif 

   ;; A Literal number
   push  eax                                 ; Push LO 32-bits
   .if   ebx == DOUBLE_DIG                       
      push edx                               ; Push HO 32-bits
   .endif

   .if  ( STATE == compileState )            ; Compile Literal?

      .if ebx == DOUBLE_DIG                  ; Double Precision
         mov   eax,OFFSET __TWOLIT.lpInterp
         call  _COMMA_DO
         pop   eax
         call  _COMMA_DO
         pop   eax
         call  _COMMA_DO
      .else                                  ; Single Precision
         mov   eax,OFFSET __LIT.lpInterp
         call  _COMMA_DO
         pop   eax
         call  _COMMA_DO
      .endif

   .endif
   NEXT

   ;; Unknown word or number
_PARSE_ERROR:
   push  esi
   invoke StdErr, OFFSET errmsg
   invoke StdErr, OFFSET wrdBuffer
   print chr$(13,10)
   pop   esi
   NEXT

_EXECUTE:
   pop   eax
   jmp  DWORD PTR [eax]


;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                    EXIT
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

   ;; Stop executing FORTH interpreter
_BYE:
   invoke ExitProcess,0   

   ;; Return from FORTH Words
_EXIT:
   POPRSP  esi
   NEXT  

;;; #########################################################################
;;;                        CONSOLE I/O PROCEDURES
;;; #########################################################################

.code

conInLen proc buff:DWORD

      mov   eax, buff
      dec   eax
   @@:
      inc   eax
      cmp   BYTE PTR [eax], 10
      jne   @B
      sub   eax,buff
      inc   eax
      ret   4

conInLen endp

;;; #########################################################################
;;;                          FILE I/O PROCEDURES
;;;
;;; slurp up to 10 files into memory, to be interpreted
;;; #########################################################################

.data
align 4
   
   NUMFILESOPEN TEXTEQU <10>

   hFileArray   dd NUMFILESOPEN dup (?)      ; Memory Files
   fileOffset   dd NUMFILESOPEN dup (?)      ; Files Offset
   fileDepth    dd 0                         ; Number of files being read

.code

   ;; Slurp file n into memory
start_file_read proc lpFile:DWORD

      LOCAL hFile:DWORD

      ;; Check maximum file depth
      .if fileDepth == NUMFILESOPEN          ; Check num open files
        print "There are too many files open!",13,10
        ret
      .endif

      ;; Slurp input file
      mov   hFile, InputFile(lpFile)

      .if ecx == 0                           ; Error reading file
         print "Error reading input file: "
         print lpFile,13,10
         ret
      .endif 

      ;; Store memory location and offset
      mov   ebx,fileDepth
      mov   eax,hFile
      mov   hFileArray[ebx],eax
      mov   fileOffset[ebx],0

      inc   fileDepth                        ; Num files open
      mov   IOMODE,fileMODE                  ; Set I/O Mode

      ret   

start_file_read endp


   ;; Stop reading from file n, and free memory
end_file_read proc

      ;; Set number files open
      dec   fileDepth         

      ;; Free memory for file
      mov   ebx,fileDepth
      mov   eax,hFileArray[ebx]
      free  eax

      ;; Switch to console mode?
      .if   ( fileDepth == 0 )          
         mov IOMODE,consoleMODE         
      .endif
 
      ret   

end_file_read endp

   ;; Return memory address and offset for file n
get_file_offset proc

      mov   ebx,fileDepth

      ;; Is there a file open?
      .if ebx == 0
        print "There are no files open!",13,10
        mov   eax,0
        ret
      .endif

      dec   ebx

      mov   eax,hFileArray[ebx]
      mov   ebx,fileOffset[ebx]

      ret

get_file_offset endp

   ;; Update the offset for reading file n
set_file_offset proc spos:DWORD

      mov   ebx,fileDepth
      dec   ebx
      
      mov   eax,spos
      mov   fileOffset[ebx],eax

      ret

set_file_offset endp

   ;; Read a line of text from file
   ;; Corrolary to StdIn
FileIn proc buffer:DWORD

      LOCAL floc:DWORD
      LOCAL spos:DWORD

      ;; Get addr/offset for memory file
      invoke get_file_offset
      mov   floc,eax
      mov   spos,ebx

   @@:
      ;; Read line from memory file, vice console
      invoke readline, floc, buffer, spos
      
      ;; End of File?
      .if eax == 0
         call end_file_read
         mov  eax,0
         ret
      .endif

      ;; Save new offset
      mov   spos,eax
      
      ;; 0 bytes returned?
      .if ecx == 0
         jmp @B
      .endif

      invoke set_file_offset, spos
      mov   eax,ecx

      ret

FileIn endp
      
      
;;; #########################################################################
;;;                           INITIALIZATION FILE
;;; #########################################################################

.data
   init_file    db ".forth",0                ;  Initialization File

.code
read_init_file proc src:DWORD

      invoke start_file_read, OFFSET init_file
      ret

read_init_file endp

;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;;;                                 MAIN PROGRAM
;;; лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
.data

   COLD_START dd OFFSET __QUIT.lpInterp

.code
start:

   invoke Main
   invoke ExitProcess,0

Main proc

      ;; Initialization file
      call read_init_file

      ;; Initialize Data Stack
      mov   S0,esp

      ;; Initialize Return Stack
      mov   ebp,rStackTop

      ;; Enter MAIN program loop
      mov   esi,OFFSET COLD_START
      NEXT  

Main endp

end start

