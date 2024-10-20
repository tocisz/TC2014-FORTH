;------------------------------------------------------------------------------
; FORTH - V0
;------------------------------------------------------------------------------
;
; This is not my source. I did not write this I only made it work on the
; RC2014 so I would have something the mess around with.
;
; I found a simple FORTH buried in a ZIP file on the Z80 info site at ...
;      http://www.z80.info/zip/z80asm.zip
;
; V0 - Included the source from Grant Searles simple 7 chip Z80 computer
;      from here http://searle.hostei.com/grant/
;      Modified the CHR_RD and CHR_WR to use the routines from INT32K.ROM
;      Added a def so I can build as a ROM or RAM
;      A few renames so I could build it with TASM.
;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; This is an implementation of FORTH for the Z80 that should be easily portable
; to other Z80 systems. It assumes RAM from 9000h to 0FFFFh and a UART for
; communication with the host or VDU.

include(`macros.m4')

DATA_STACK:	.EQU	0FD80h		;Data stack grows down
VOCAB_BASE:	.EQU	0F000h		;Dictionary grows up from here
MASS_STORE:	.EQU	0FEA0h		;Mass storage buffer (default)
DISK_START:	.EQU	0A000h		;Pseudo disk buffer start
DISK_END:	.EQU	0F000h		;Pseudo disk buffer end
BLOCK_SIZE:	.EQU	0200h		;Pseudo disk block size
BUFFERS:	.EQU	0001h		;Pseudo disk buffers per block

MAX_DISK_BLOCKS = (DISK_END-DISK_START)/BLOCK_SIZE

; Start of FORTH code

.section .cold

	JP	X_COLD

.section .warm

	JP	C_WARM

BACKSPACE:
	.WORD	0008h			;Backspace chr

WORD1: ; copied by C_WARM to S0 and onward
	.WORD	DATA_STACK
DEF_SYSADDR:
	.WORD	SYSTEM
	.WORD	DATA_STACK
	.WORD	001Fh			;Word name length (default 31)
	.WORD	0000h			;Error message control number
	.WORD	VOCAB_BASE		;FORGET protection
	.WORD	VOCAB_BASE+(W_TASKEND-W_TASK) ;Dictionary pointer -- this must be >= length of a last word
	.WORD	E_FORTH			;Most recently created vocab.

START_TABLE:
	.BYTE	81h,0A0h
	.WORD	VOCAB_BASE
	.BYTE	00h,00h			;FLAST
	.BYTE	81h,0A0h
	.WORD	W_EDITI
	.WORD	E_FORTH			;ELAST
	.BYTE	00h			;CRFLAG
	.BYTE	00h			;Free
	IN	A,(00h)			;I/O Port input
	RET				;routine
	OUT	(00h),A			;I/O Port output
	RET				;routine
	.WORD	SYSTEM 			;Return stack pointer
	.WORD	MASS_STORE		;Mass storage buffer to use
	.WORD	MASS_STORE		;Storage buffer just used
	.BYTE	00h			;Interrupt flag
	.BYTE	00h			;Free
	.WORD	C_ABORT			;Interrupt vector
	.WORD	CF_UQTERMINAL		;C field address ?TERMINAL
	.WORD	CF_UKEY			;C field address KEY
	.WORD	CF_UEMIT		;C field address EMIT
	.WORD	CF_UCR			;C field address CR
	.WORD	CF_URW			;C field address R/W
	.WORD	CF_UABORT		;C field address ABORT
	.WORD	0020h			;CHRs per input line
	.WORD	DISK_START		;Pseudo disk buf start
	.WORD	DISK_END		;Pseudo disk buf end
	.WORD	BLOCK_SIZE		;Bytes per block
	.WORD	BUFFERS			;Buffers per block

NEXTS2:
	PUSH	DE
NEXTS1:
	PUSH	HL
NEXT:
	LD	A,(INTFLAG)		;Interrupt flag
	BIT	7,A				;Check for interrupt
	JR	Z,NOINT			;No interrupt
	BIT	6,A				;Interrupt enabled ?
	JR	NZ,NOINT		;No interrupt
	LD	HL,(INTVECT)	;Get interrupt vector
	LD	A,40h			;Clear flag byte
	LD	(INTFLAG),A		;Interrupt flag into HL
	JR	NEXTADDR		;JP (HL)
NOINT:
	LD	A,(BC)			;effectively LD HL,(BC)
	INC	BC			;
	LD	L,A			;
	LD	A,(BC)			;
	INC	BC			;BC now points to next vector
	LD	H,A			;HL has addr vector
NEXTADDR:
	LD	E,(HL)			;effectively LD HL,(HL)
	INC	HL			;
	LD	D,(HL) 			;
	EX	DE,HL 			;
	JP	(HL) 			;Jump to it

.set last_word_address, 0000h ;First word in vocabulary

W_LIT:					;Puts next 2 bytes on the stack
	define_word(`lit')
C_LIT:
	.WORD	2+$			;Vector to code
	LD	A,(BC)			;Gets next word from (BC)
	INC	BC			;then increments BC to point
	LD	L,A			;to the next addr. Pushes the
	LD	A,(BC)			;result onto the stack.
	INC	BC			;
	LD	H,A			;
	JP	NEXTS1			;Save & NEXT

W_EXECUTE:				;Jump to address on stack
	define_word(`execute')
C_EXECUTE:
	.WORD	2+$			;Vector to code
	POP	HL			;Get addr off data stack
	JP	NEXTADDR		;Basically JP (HL)

W_BRANCH:				;Add following offset to BC
	define_word(`branch')
C_BRANCH:
	.WORD	2+$			;Vector to code
X_BRANCH:
	LD	H,B			;Next pointer into HL
	LD	L,C			;
	LD	E,(HL)			;Get word offset LD DE,(HL)
	INC	HL			;Incr to point at next byte
	LD	D,(HL)			;
	DEC	HL 			;Restore HL
	ADD	HL,DE			;Calculate new address
	LD	C,L			;Put it in BC
	LD	B,H			;
	JP	NEXT			;Go do it

W_0BRANCH:				;Add offset to BC if stack top = 0
	define_word(`0branch')
C_0BRANCH:
	.WORD	2+$			;Vector to code
	POP	HL			;Get value off stack
	LD	A,L			;Set flags
	OR	H			;
	JR	Z,X_BRANCH		;If zero then do the branch
	INC	BC			;Else dump branch address
	INC	BC			;
	JP	NEXT			;Continue execution

W_LLOOP:				;Increment loop & branch if not done
	define_word(`<loop>')
C_LLOOP:
	.WORD	2+$			;Vector to code
	LD	DE,0001
C_ILOOP:
	LD	HL,(RPP)		;Get return stack pointer
	LD	A,(HL)			;Add DE to value on return stack
	ADD	A,E			;
	LD	(HL),A			;
	LD	E,A			;
	INC	HL			;
	LD	A,(HL)			;
	ADC	A,D			;
	LD	(HL),A			;
	INC	HL			;HL now points to limit value
	INC	D			;Get DS sign bit
	DEC	D			;
	LD	D,A			;Result now in DE
	JP	M,DECR_LOOP		;Decrement loop so check > limit
					;otherwies check < limit
	LD	A,E			;Low byte back
	SUB	(HL)			;Subtract limit low
	LD	A,D			;High byte back
	INC	HL			;Point to limit high
	SBC	A,(HL)			;Subtract it
	JR	TEST_LIMIT		;
DECR_LOOP:
	LD	A,(HL)			;Get limit low
	SUB	E			;Subtract index low
	INC	HL			;Point to limit high
	LD	A,(HL)			;Get it
	SBC	A,D			;Subtract index high
TEST_LIMIT:
	JP	M,X_BRANCH		;Not reached limit so jump
	INC	HL			;Drop index & limit from return stack
	LD	(RPP),HL		;Save stack pointer
	INC	BC			;Skip branch offset
	INC	BC			;
	JP	NEXT

W_PLOOP:				;Loop + stack & branch if not done
	define_word(`<+loop>')
C_PLOOP:
	.WORD	2+$			;Vector to code
	POP	DE			;Get value from stack
	JR	C_ILOOP			;Go do loop increment

W_LDO:					;Put start & end loop values on RPP
	define_word(`<do>')
C_LDO:
	.WORD	 2+$
	LD	HL,(RPP)		;Get return stack pointer
	DEC	HL			;Add space for two values
	DEC	HL			;
	DEC	HL			;
	DEC	HL			;
	LD	(RPP),HL		;Save new stack pointer
	POP	DE			;Get start value &
	LD	(HL),E			;put on return stack top
	INC	HL			;
	LD	(HL),D			;
	INC	HL			;
	POP	DE			;Get end value &
	LD	(HL),E			;put on return stack - 1
	INC	HL			;
	LD	(HL),D			;
	JP	NEXT

W_I:					;Copy LOOP index to data stack
	define_word(`i')
C_I:
	.WORD	 2+$
X_I:
	LD	HL,(RPP)		;Get return stack pointer
X_I2:
	LD	E,(HL)			;Get LOOP index off return stack
	INC	HL			;
	LD	D,(HL)			;
	PUSH	DE			;Push onto data stack
	JP	NEXT

W_DIGIT:				;Convert digit n2 using base n1
	define_word(`digit')
C_DIGIT:
	.WORD	2+$
	POP	HL			;Get base to use
	POP	DE			;Get char
	LD	A,E			;A = char
	SUB	30h			;Subtract 30h
	JP	M,NDIGIT		;
	CP	0Ah			;Greater than 9 ?
	JP	M,LESS10		;If not then skip
	SUB	07h			;Convert 'A' to 10
	CP	0Ah			;Is it 10?
	JP	M,NDIGIT		;If not an error occured
LESS10:
	CP	L			;L is 1 digit limit
	JP	P,NDIGIT		;Out of range for digit
	LD	E,A			;Result into DE
	LD	HL,0001			;Leave TRUE flag
	JP	NEXTS2			;Save both & NEXT
NDIGIT:
	LD	L,H			;Leave FALSE flag
	JP	NEXTS1			;Save & NEXT

W_FIND:					;Find word & return vector,byte & flag
	define_word(`<find>')
C_FIND:
	.WORD	2+$			;Vector to code
	POP	DE			;Get pointer to next vocabulary word
COMPARE:
	POP	HL			;Copy pointer to word we're looking 4
	PUSH	HL			;
	LD	A,(DE)			;Get 1st vocabulary word letter
	XOR	(HL)			;Compare with what we've got
	AND	3Fh			;Ignore start flag
	JR	NZ,NOT_END_CHR		;No match so skip to next word
MATCH_NO_END:
	INC	HL			;Compare next chr
	INC	DE			;
	LD	A,(DE)			;
	XOR	(HL)			;
	ADD	A,A			;Move bit 7 to C flag
	JR	NZ,NO_MATCH		;No match jump
	JR	NC,MATCH_NO_END		;Match & not last, so next chr
	LD	HL,0005			;Offset to start of code
	ADD	HL,DE			;HL now points to code start for word
	EX	(SP),HL			;Swap with value on stack
NOT_WORD_BYTE:
	DEC	DE			;Search back for word type byte
	LD	A,(DE)			;
	OR	A			;
	JP	P,NOT_WORD_BYTE		;Not yet so loop
	LD	E,A			;Byte into DE
	LD	D,00			;
	LD	HL,0001			;Leave TRUE flag
	JP	NEXTS2			;Save both & NEXT
NO_MATCH:
	JR	C,END_CHR		;If last chr then jump
NOT_END_CHR:
	INC	DE			;Next chr of this vocab word
	LD	A,(DE)			;Get it
	OR	A			;Set flags
	JP	P,NOT_END_CHR		;Loop if not end chr
END_CHR:
	INC	DE			;Now points to next word vector
	EX	DE,HL			;Swap
	LD	E,(HL)			;Vector into DE
	INC	HL			;
	LD	D,(HL)			;
	LD	A,D			;Check it's not last (first) word
	OR	E			;
	JR	NZ,COMPARE		;No error so loop
	POP	HL			;Dump pointer
	LD	HL,0000			;Flag error
	JP	NEXTS1			;Save & NEXT

W_ENCLOSE:
	define_word(`enclose')
C_ENCLOSE:
	.WORD	2+$			;Vector to code
	POP	DE			; get delimiter character
	POP	HL			; get address 1
	PUSH	HL			; duplicate it
	LD	A,E			; delimiter char into A
	LD	D,A			; copy to D
	LD	E,00FFh			; -1 for offset
	DEC	HL			; to allow for first INCR
J21E6:
	INC	HL			; point to next chr
	INC	E			; next offset
	CP	(HL)			; compare chr with (address)
	JR	Z,J21E6			; loop if = delimiter chr
	LD	A,0Dh			; else set CR
	CP	(HL)			; compare with (address)
	LD	A,D			; restore delimiter chr
	JR	Z,J21E6			; loop if it was = CR
	LD	D,00h			; zero high byte
	PUSH	DE			; save offset
	LD	D,A			; restore delimiter chr
	LD	A,(HL)			; get byte from address
	AND	A			; set the flags
	JR	NZ,J2202		; branch if not null
	LD	D,00h			; clear high byte
	INC	E			; point to next addr
	PUSH	DE			; save address
	DEC	E			; point to end
	PUSH	DE			; push address
	JP	NEXT			; done
J2202:
	LD	A,D			; restore delimiter chr
	INC	HL			; increment address
	INC	E			; increment offset
	CP	(HL)			; compare delimiter with (address)
	JR	Z,J2218			; jump if =
	LD	A,0Dh			; else get CR
	CP	(HL)			; compare with (address)
	JR	Z,J2218			; jump if =
	LD	A,(HL)			; else get byte
	AND	A			; set the flags
	JR	NZ,J2202		; loop if not null
	LD	D,00h			; clear gigh byte
	PUSH	DE			; save address
	PUSH	DE			; save address
	JP	NEXT			; done
J2218:
	LD	D,00h			; clear high byte
	PUSH	DE			; save address
	INC	E			; increment offset
	PUSH	DE			; save address
	JP	NEXT			; done

W_EMIT:					;Output CHR from stack
	define_word(`emit')
C_EMIT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_UEMIT			;Put UEMIT addr on stack
	.WORD	C_FETCH			;Get UEMIT code field address
	.WORD	C_EXECUTE		;Jump to address on stack
	.WORD	C_1
	.WORD	C_OUT
	.WORD	C_PLUSSTORE
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_KEY:					;Wait for key, value on stack
	define_word(`key')
C_KEY:
	.WORD	2+$			;Vector to code
	LD	HL,(UKEY)		;Get the vector
	JP	(HL)			;Jump to it

W_TERMINAL:
	define_word(`?terminal')
C_TERMINAL:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_UTERMINAL
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_EXECUTE		;Jump to address on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CR:					    ;Output [CR][LF]
	define_word(`cr')
C_CR:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_UCR			;Push UCR addr
	.WORD	C_FETCH			;Get UCR code field addr
	.WORD	C_EXECUTE		;Jump to address on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CLS:					    ;Clear screen
	define_word(`cls')
C_CLS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Put clear screen code on stack
	.WORD	000Ch			;
	.WORD	C_EMIT			;Output it
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CMOVE:				;Move block
	define_word(`cmove')
C_CMOVE:
	.WORD	2+$			;Vector to code
	LD	L,C			;Save BC for now
	LD	H,B			;
	POP	BC			;Get no. of bytes to move
	POP	DE			;Get destination address
	EX	(SP),HL			;Get source address
	LD	A,B			;Check it's not a 0 length block
	OR	C			;
	JR	Z,NO_BYTES		;If 0 length then do nothing
	LDIR				;Move block
NO_BYTES:
	POP	BC			;Get BC back
	JP	NEXT

W_USTAR:				;Unsigned multiply
	define_word(`u*')
C_USTAR:
	.WORD	2+$			;Vector to code
	POP	DE			; get n2
	POP	HL			; get n1
	PUSH	BC			; save BC for now
	LD	C,H			; save H
	LD	A,L			; low byte to multiply by
	CALL	HALF_TIMES		; HL = A * DE
	PUSH	HL			; save partial result
	LD	H,A			; clear H
	LD	A,C			; high byte to multiply by
	LD	C,H			; clear B
	CALL	HALF_TIMES		; HL = A * DE
	POP	DE			; get last partial result
	LD	B,C			; add partial results
	LD	C,D			; add partial results
	ADD	HL,BC			;
	ADC	A,00h			;
	LD	D,L			;
	LD	L,H			;
	LD	H,A			;
	POP	BC			; get BC back
	JP	NEXTS2			; save 32 bit result & NEXT

HALF_TIMES:				;
	LD	HL,0000h		; clear partial result
	LD	B,08h			; eight bits to do
NEXT_BIT:
	ADD	HL,HL			; result * 2
	RLA				; multiply bit into C
	JR	NC,NO_MUL		; branch if no multiply
	ADD	HL,DE			; add multiplicand
	ADC	A,00h			; add in any carry
NO_MUL:
	DJNZ	NEXT_BIT		; decr and loop if not done
	RET				;

W_UMOD:					;Unsigned divide & MOD
	define_word(`u/mod')
C_UMOD:
	.WORD	2+$			;Vector to code
	LD	HL,0004
	ADD	HL,SP
	LD	E,(HL)
	LD	(HL),C
	INC	HL
	LD	D,(HL)
	LD	(HL),B
	POP	BC
	POP	HL
	LD	A,L
	SUB	C
	LD	A,H
	SBC	A,B
	JR	C,J22.BYTE
	LD	HL,0FFFFh
	LD	DE,0FFFFh
	JR	J2301
J22.BYTE:
	LD	A,10h
J22DD:
	ADD	HL,HL
	RLA
	EX	DE,HL
	ADD	HL,HL
	JR	NC,J22E5
	INC	DE
	AND	A
J22E5:
	EX	DE,HL
	RRA
	PUSH	AF
	JR	NC,J22F2
	LD	A,L
	SUB	C
	LD	L,A
	LD	A,H
	SBC	A,B
	LD	H,A
	JR	J22FC
J22F2:
	LD	A,L
	SUB	C
	LD	L,A
	LD	A,H
	SBC	A,B
	LD	H,A
	JR	NC,J22FC
	ADD	HL,BC
	DEC	DE
J22FC:
	INC	DE
	POP	AF
	DEC	A
	JR	NZ,J22DD
J2301:
	POP	BC
	PUSH	HL
	PUSH	DE
	JP	NEXT

W_AND:					;AND
	define_word(`and')
C_AND:
	.WORD	2+$			;Vector to code
	POP	DE			;Get n1 off stack
	POP	HL			;Get n2 off stack
	LD	A,E			;AND lo bytes
	AND	L			;
	LD	L,A			;Result in L
	LD	A,D			;AND hi bytes
	AND	H			;
	LD	H,A			;Result in H
	JP	NEXTS1			;Save & next

W_OR:					;OR
	define_word(`or')
C_OR:
	.WORD	2+$			;Vector to code
	POP	DE			;Get n1 off stack
	POP	HL			;Get n2 off stack
	LD	A,E			;OR lo bytes
	OR	L			;
	LD	L,A			;Result in L
	LD	A,D			;OR hi bytes
	OR	H			;
	LD	H,A			;Result in H
	JP	NEXTS1			;Save & next

W_XOR:					;XOR
	define_word(`xor')
C_XOR:
	.WORD	2+$			;Vector to code
	POP	DE			;Get n1 off stack
	POP	HL			;Get n2 off stack
	LD	A,E			;XOR lo bytes
	XOR	L			;
	LD	L,A			;Result in L
	LD	A,D			;XOR hi bytes
	XOR	H			;
	LD	H,A			;Result in H
	JP	NEXTS1			;Save & NEXT

W_SPFETCH:				;Stack pointer onto stack
	define_word(`sp@')
C_SPFETCH:
	.WORD	2+$			;Vector to code
	LD	HL,0000			;No offset
	ADD	HL,SP			;Add SP to HL
	JP	NEXTS1			;Save & NEXT

W_SPSTORE:				;Set initial stack pointer value
	define_word(`sp!')
C_SPSTORE:
	.WORD	2+$			;Vector to code
	LD	HL,(DEF_SYSADDR)	;Get system base addr
	LD	DE,S0-SYSTEM		;Offset to stack pointer value (0006)
	ADD	HL,DE			;Add to base addr
	LD	E,(HL)			;Get SP from ram
	INC	HL			;
	LD	D,(HL)			;
	EX	DE,HL			;Put into HL
	LD	SP,HL			;Set SP
	JP	NEXT

W_RPFETCH:				;Get return stack pointer
	define_word(`rp@')
C_RPFETCH:
	.WORD	2+$			;Vector to code
	LD	HL,(RPP)		;Return stack pointer into HL
	JP	NEXTS1			;Save & NEXT

W_RPSTORE:				;Set initial return stack pointer
	define_word(`rp!')
C_RPSTORE:
	.WORD	2+$			;Vector to code
	LD	HL,(DEF_SYSADDR)	;Get system base addr
	LD	DE,R0-SYSTEM	;Offset to return stack pointer value
	ADD	HL,DE			;Add to base addr
	LD	E,(HL)			;Get SP from ram
	INC	HL			;
	LD	D,(HL)			;
	EX	DE,HL			;Put into HL
	LD	(RPP),HL		;Set return SP
	JP	NEXT

W_STOP:					;Pop BC from return stack (=next)
	define_word(`;s')
C_STOP:
	.WORD	2+$			;Vector to code
X_STOP:
	LD	HL,(RPP)		;Return stack pointer to HL
	LD	C,(HL)			;Get low byte
	INC	HL			;
	LD	B,(HL)			;Get high byte
	INC	HL			;
	LD	(RPP),HL		;Save stack pointer
	JP	NEXT

W_LEAVE:				;Quit loop by making index = limit
	define_word(`leave')
C_LEAVE:
	.WORD	2+$			;Vector to code
	LD	HL,(RPP)		;Get return stack pointer
	LD	E,(HL)			;Get loop limit low
	INC	HL			;
	LD	D,(HL)			;Get loop limit high
	INC	HL			;
	LD	(HL),E			;Set index low to loop limit
	INC	HL			;
	LD	(HL),D			;Set index high to loop limit
	JP	NEXT

W_MOVER:				;Move from data to return stack
	define_word(`>r')
C_MOVER:
	.WORD	2+$			;Vector to code
	POP	DE			;Get value
	LD	HL,(RPP)		;Get return stack pointer
	DEC	HL			;Set new value
	DEC	HL			;
	LD	(RPP),HL		;Save it
	LD	(HL),E			;Push low byte onto return stack
	INC	HL			;
	LD	(HL),D			;Push high byte onto return stack
	JP	NEXT

W_RMOVE:				;Move word from return to data stack
	define_word(`r>')
C_RMOVE:
	.WORD	2+$			;Vector to code
	LD	HL,(RPP)		;Get return stack pointer
	LD	E,(HL)			;Pop word off return stack
	INC	HL			;
	LD	D,(HL)			;
	INC	HL			;
	LD	(RPP),HL		;Save new return stack pointer
	PUSH	DE			;Push on data stack
	JP	NEXT

W_RFETCH:				;Return stack top to data stack
	define_word(`r@')
C_RFETCH:
	.WORD	X_I			;Return stack top to data stack

W_0EQUALS:				;=0
	define_word(`0=')
C_0EQUALS:
	.WORD	2+$			;Vector to code
X_0EQUALS:
	POP	HL			;Get value from stack
	LD	A,L			;set flags
	OR	H			;
	LD	HL,0000			;Not = 0 flag
	JR	NZ,NO_ZERO		;
	INC	HL			;= 0 flag
NO_ZERO:
	JP	NEXTS1			;Save & NEXT

W_NOT:					;Convert flag, same as 0=
	define_word(`not')
C_NOT:
	.WORD	X_0EQUALS

W_0LESS:				;Less than 0
	define_word(`0<')
C_0LESS:
	.WORD	2+$			;Vector to code
	POP	HL			;Get value
	ADD	HL,HL			;S bit into C
	LD	HL,0000			;Wasn't < 0 flag
	JR	NC,NOT_LT0		;
	INC	HL			;Was < 0 flag
NOT_LT0:				;
	JP	NEXTS1			;Save & NEXT

W_PLUS:					;n1 + n2
	define_word(`+')
C_PLUS:
	.WORD	2+$			;Vector to code
	POP	DE			;Get n2
	POP	HL			;Get n1
	ADD	HL,DE			;Add them
	JP	NEXTS1			;Save & NEXT

W_DPLUS:				;32 bit add
	define_word(`d+')
C_DPLUS:
	.WORD	2+$			;Vector to code
	LD	HL,0006			; offset to low word
	ADD	HL,SP			; add stack pointer
	LD	E,(HL)			; get d1 low word low byte
	LD	(HL),C			; save BC low byte
	INC	HL			; point to high byte
	LD	D,(HL)			; get d1 low word high byte
	LD	(HL),B			; save BC high byte
	POP	BC			; get high word d2
	POP	HL			; get low word d2
	ADD	HL,DE			; add low wor.BLOCK
	EX	DE,HL			; save result low word in DE
	POP	HL			; get d1 high word
	LD	A,L			; copy d1 high word low byte
	ADC	A,C			; add d2 high word low byte
					; + carry from low word add
	LD	L,A			; result from high word low byte into L
	LD	A,H			; copy d1 high word low byte
	ADC	A,B			; add d2 high word low byte
					; + carry from high word low byte add
	LD	H,A			; result from high word high byte into H
	POP	BC			; restore BC
	JP	NEXTS2			;Save 32 bit result & NEXT

W_NEGATE:				;Form 2s complement of n
	define_word(`negate')
C_NEGATE:
	.WORD	2+$			;Vector to code
	POP	HL			;Get number
	LD	A,L			;Low byte into A
	CPL				;Complement it
	LD	L,A			;Back into L
	LD	A,H			;High byte into A
	CPL				;Complement it
	LD	H,A			;Back into H
	INC	HL			;+1
	JP	NEXTS1			;Save & NEXT

W_DNEGATE:				;Form 2s complement of 32 bit n
	define_word(`dnegate')
C_DNEGATE:
	.WORD	2+$			;Vector to code
	POP	HL			; get high word
	POP	DE			; get low word
	SUB	A			; clear A
	SUB	E			; negate low word low byte
	LD	E,A			; copy back to E
	LD	A,00h			; clear A
	SBC	A,D			; negate low word high byte
	LD	D,A			; copy back to D
	LD	A,00h			; clear A
	SBC	A,L			; negate high word low byte
	LD	L,A			; copy back to L
	LD	A,00h			; clear A
	SBC	A,H			; negate high word high byte
	LD	H,A			; copy back to H
	JP	NEXTS2			;Save 32 bit result & NEXT

W_OVER:					;Copy 2nd down to top of stack
	define_word(`over')
C_OVER:
	.WORD	2+$			;Vector to code
	POP	DE			;Get top
	POP	HL			;Get next
	PUSH	HL			;Save it back
	JP	NEXTS2			;Save both & NEXT

W_DROP:					;Drop top value from stack
	define_word(`drop')
C_DROP:
	.WORD	2+$			;Vector to code
	POP	HL			;Get top value
	JP	NEXT

W_2DROP:				;Drop top two values from stack
	define_word(`2drop')
C_2DROP:
	.WORD	2+$			;Vector to code
	POP	HL			;Get top value
	POP	HL			;Get top value
	JP	NEXT

W_SWAP:					;Swap top 2 values on stack
	define_word(`swap')
C_SWAP:
	.WORD	2+$			;Vector to code
	POP	HL			;Get top value
	EX	(SP),HL			;Exchanhe with next down
	JP	NEXTS1			;Save & NEXT

W_DUP:					;Duplicate top value on stack
	define_word(`dup')
C_DUP:
	.WORD	2+$			;Vector to code
	POP	HL			;Get value off stack
	PUSH	HL			;Copy it back
	JP	NEXTS1			;Save & NEXT

W_2DUP:					;Dup top 2 values on stack
	define_word(`2dup')
C_2DUP:
	.WORD	2+$			;Vector to code
	POP	HL			;Get top two values from stack
	POP	DE			;
	PUSH	DE			;Copy them back
	PUSH	HL			;
	JP	NEXTS2			;Save both & NEXT

W_BOUNDS:				;Convert address & n to start & end
	define_word(`bounds')
C_BOUNDS:
	.WORD	2+$			;Vector to code
	POP	HL			; get n
	POP	DE			; get addr
	ADD	HL,DE			; add addr to n
	EX	DE,HL			; swap them
	JP	NEXTS2			; save both & NEXT

W_PLUSSTORE:				;Add n1 to addr
	define_word(`+!')
C_PLUSSTORE:
	.WORD	2+$			;Vector to code
	POP	HL			;Get addr
	POP	DE			;Get DE
	LD	A,(HL)			;Add low bytes
	ADD	A,E			;
	LD	(HL),A			;Store result
	INC	HL			;Point to high byte
	LD	A,(HL)			;Add high bytes
	ADC	A,D			;
	LD	(HL),A			;Store result
	JP	NEXT

W_TOGGLE:				;XOR (addr) with byte
	define_word(`toggle')
C_TOGGLE:
	.WORD	2+$			;Vector to code
	POP	DE			 	;Get byte
	POP	HL				;Get addr
	LD	A,(HL)			;Get byte from addr
	XOR	E				;Toggle it
	LD	(HL),A			;Save result
	JP	NEXT

W_FETCH:				;Get word from addr on stack
	define_word(`@')
C_FETCH:
	.WORD	2+$			;Vector to code
	POP	HL			;Get addr
	LD	E,(HL)			;Get low byte
	INC	HL			;
	LD	D,(HL)			;Get high byte
	PUSH	DE			;Save it
	JP	NEXT

W_CFETCH:				;Get byte from addr on stack
	define_word(`c@')
C_CFETCH:
	.WORD	2+$			;Vector to code
	POP	HL			;Get addr
	LD	L,(HL)			;Get byte
	LD	H,00h			;Top byte = 0
	JP	NEXTS1			;Save & NEXT

W_2FETCH:				;Get word from addr+2 and addr
	define_word(`2@')
C_2FETCH:
	.WORD	2+$			;Vector to code
	POP	HL			;Get addr
	LD	DE,0002			;Plus 2 bytes
	ADD	HL,DE			;Get 2nd word first
	LD	E,(HL)			;Low byte
	INC	HL			;
	LD	D,(HL)			;High byte
	PUSH	DE			;Save it
	LD	DE,0FFFDh		;Minus 2 bytes
	ADD	HL,DE			;Get 1st word
	LD	E,(HL)			;Low byte
	INC	HL			;
	LD	D,(HL)			;High byte
	PUSH	DE			;Save it
	JP	NEXT

W_STORE:				;Store word at addr
	define_word(`!')
C_STORE:
	.WORD	2+$			;Vector to code
	POP	HL			;Get addr
	POP	DE			;Get word
	LD	(HL),E			;Store low byte
	INC	HL			;
	LD	(HL),D			;Store high byte
	JP	NEXT

W_CSTORE:				;Store byte at addr
	define_word(`c!')
C_CSTORE:
	.WORD	2+$			;Vector to code
	POP	HL			;Get addr
	POP	DE			;Get byte
	LD	(HL),E			;Save it
	JP	NEXT

W_2STORE:				;Store 2 words at addr (+2)
	define_word(`2!')
C_2STORE:
	.WORD	2+$			;Vector to code
	POP	HL			;Get addr
	POP	DE			;Get word
	LD	(HL),E			;Save low byte
	INC	HL			;
	LD	(HL),D			;Save high byte
	INC	HL			;
	POP	DE			;Get next word
	LD	(HL),E			;Save low byte
	INC	HL			;
	LD	(HL),D			;Save high byte
	JP	NEXT

W_COLON:
	define_word(`:')
C_COLON:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_QEXEC			;Error not if not in execute mode
	.WORD	C_CSPSTORE		;Set current stack pointer value
	.WORD	C_CURRENT		;Get CURRENT addr
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_CONTEXT		;Make CONTEXT current vocab
	.WORD	C_STORE			;Store word at addr
	.WORD	C_XXX1			;Puts name into dictionary
	.WORD	C_RIGHTBRKT		;Set STATE to compile
	.WORD	C_CCODE			;Execute following machine code

E_COLON:
	LD	HL,(RPP)		;Get return stack pointer
	DEC	HL			;Put BC on return stack
	LD	(HL),B			;
	DEC	HL			;
	LD	(HL),C			;
	LD	(RPP),HL		;Save new pointer
	INC	DE
	LD	C,E
	LD	B,D
	JP	NEXT

W_SEMICOLON:				;Terminate compilation
	define_immediate_word(`;')
C_SEMICOLON:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_QCOMP			;Check we're allready compiling
	.WORD	C_WHATSTACK		;Check stack pointer, error if not ok
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	C_STOP			;
	.WORD	C_SMUDGE		;Smudge bit to O.K.
	.WORD	C_LEFTBRKT		;Set STATE to execute
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CONSTANT:
	define_word(`constant')
C_CONSTANT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_XXX1
	.WORD	C_SMUDGE
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_CCODE			;Execute following machine code

X_CONSTANT:				;Put next word on stack
	INC	DE			;Adjust pointer
	EX	DE,HL			;Get next word
	LD	E,(HL)			;
	INC	HL			;
	LD	D,(HL)			;
	PUSH	DE			;Put on stack
	JP	NEXT

W_VARIABLE:
	define_word(`variable')
C_VARIABLE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_CONSTANT
	.WORD	C_CCODE			;Execute following machine code

X_VARIABLE:
	INC	DE
	PUSH	DE
	JP	NEXT

W_USER:
	define_word(`user')
C_USER:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_CONSTANT
	.WORD	C_CCODE			;Execute following machine code

X_USER:
	INC	DE			;Adjust to next word
	EX	DE,HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	LD	HL,(DEF_SYSADDR)
	ADD	HL,DE
	JP	NEXTS1			;Save & NEXT

W_ZERO:					;Put zero on stack
	define_word(`0')
C_ZERO:
	.WORD	X_CONSTANT		;Put next word on stack
	.WORD	0000h

W_1:					;Put 1 on stack
	define_word(`1')
C_1:
	.WORD	X_CONSTANT		;Put next word on stack
	.WORD	0001h

W_2:
	define_word(`2')
C_2:
	.WORD	X_CONSTANT		;Put next word on stack
	.WORD	0002h

W_3:
	define_word(`3')
C_3:
	.WORD	X_CONSTANT		;Put next word on stack
	.WORD	0003h

W_BL:					;Leaves ASCII for blank on stack
	define_word(`bl')
C_BL:
	.WORD	X_CONSTANT		;Put next word on stack
	.WORD	0020h

W_CL:
	define_word(`c/l')
C_CL:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_UCL
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_FIRST:
	define_word(`first')
C_FIRST:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_UFIRST		;Put UFIRST addr on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_LIMIT:
	define_word(`limit')
C_LIMIT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ULIMIT		;Put ULIMIT on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_BBUF:
	define_word(`b/buf')
C_BBUF:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_UBBUF
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_BSCR:
	define_word(`b/scr')
C_BSCR:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_UBSCR			;Number of buffers per block
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_S0:					;Push S0 (initial data stack pointer)
	define_word(`s0')
C_S0:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	S0-SYSTEM

W_R0:
	define_word(`r0')
C_R0:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	R0-SYSTEM

W_TIB:
	define_word(`tib')
C_TIB:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	TIB-SYSTEM

W_WIDTH:
	define_word(`width')
C_WIDTH:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	WIDTH-SYSTEM

W_WARNING:				;Put WARNING addr on stack
	define_word(`warning')
C_WARNING:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	WARNING-SYSTEM

W_FENCE:
	define_word(`fence')
C_FENCE:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	FENCE-SYSTEM

W_DP:					;Dictionary pointer addr on stack
	define_word(`dp')
C_DP:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	DP-SYSTEM

W_VOC_LINK:
	define_word(`voc-link')
C_VOC_LINK:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	VOC_LINK-SYSTEM

W_BLK:
	define_word(`blk')
C_BLK:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	BLK-SYSTEM

W_TOIN:
	define_word(`>in')
C_TOIN:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	TOIN-SYSTEM

W_OUT:					;Put OUT buffer count addr on stack
	define_word(`out')
C_OUT:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	OUT-SYSTEM

W_SCR:
	define_word(`scr')
C_SCR:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	SCR-SYSTEM

W_OFFSET:				;Put disk block offset on stack
	define_word(`offset')
C_OFFSET:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	OFFSET-SYSTEM

W_CONTEXT:
	define_word(`context')
C_CONTEXT:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	CONTEXT-SYSTEM

W_CURRENT:
	define_word(`current')
C_CURRENT:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	CURRENT-SYSTEM

W_STATE:				;Push STATE addr
	define_word(`state')
C_STATE:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	STATE-SYSTEM

W_BASE:					;Put BASE addr on stack
	define_word(`base')
C_BASE:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	BASE-SYSTEM

W_DPL:
	define_word(`dpl')
C_DPL:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	DPL-SYSTEM

W_FLD:
	define_word(`fld')
C_FLD:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	FLD-SYSTEM

W_CSP:					;Push check stack pointer addr
	define_word(`csp')
C_CSP:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	CSP-SYSTEM

W_RHASH:
	define_word(`r#')
C_RHASH:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RHASH-SYSTEM

W_HLD:
	define_word(`hld')
C_HLD:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	HLD-SYSTEM

W_UCL:
	define_word(`uc/l')
C_UCL:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	UCL-SYSTEM

W_UFIRST:
	define_word(`ufirst')
C_UFIRST:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	UFIRST-SYSTEM

W_ULIMIT:
	define_word(`ulimit')
C_ULIMIT:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	ULIMIT-SYSTEM

W_UBBUF:
	define_word(`ub/buf')
C_UBBUF:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	UBBUF-SYSTEM

W_UBSCR:
	define_word(`ub/scr')
C_UBSCR:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	UBSCR-SYSTEM

W_UTERMINAL:
	define_word(`u?terminal')
C_UTERMINAL:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	UTERMINAL-SYSTEM

W_UKEY:					;Put UKEY addr on stack
	define_word(`ukey')
C_UKEY:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	UKEY-SYSTEM

W_UEMIT:				;Put UEMIT addr on stack
	define_word(`uemit')
C_UEMIT:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	UEMIT-SYSTEM

W_UCR:					;Push UCR addr
	define_word(`ucr')
C_UCR:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	UCR-SYSTEM

W_URW:
	define_word(`ur/w')
C_URW:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	URW-SYSTEM

W_UABORT:				;Put UABORT on stack
	define_word(`uabort')
C_UABORT:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	UABORT-SYSTEM

W_RAF:
	define_word(`raf')
C_RAF:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RAF-SYSTEM

W_RBC:
	define_word(`rbc')
C_RBC:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RBC-SYSTEM

W_RDE:
	define_word(`rde')
C_RDE:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RDE-SYSTEM

W_RHL:
	define_word(`rhl')
C_RHL:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RHL-SYSTEM

W_RIX:
	define_word(`rix')
C_RIX:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RIX-SYSTEM

W_RIY:
	define_word(`riy')
C_RIY:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RIY-SYSTEM

W_RAF2:
	define_comma_word(`raf')
C_RAF2:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RAF2-SYSTEM

W_RBC2:
	define_comma_word(`rbc')
C_RBC2:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RBC2-SYSTEM

W_RDE2:
	define_comma_word(`rde')
C_RDE2:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RDE2-SYSTEM

W_RHL2:
	define_comma_word(`rhl')
C_RHL2:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RHL2-SYSTEM

W_RA:
	define_word(`ra')
C_RA:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RAF+1-SYSTEM

W_RF:
	define_word(`rf')
C_RF:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RAF-SYSTEM

W_RB:
	define_word(`rb')
C_RB:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RBC+1-SYSTEM

W_RC:
	define_word(`rc')
C_RC:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RBC-SYSTEM

W_RD:
	define_word(`rd')
C_RD:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RDE+1-SYSTEM

W_RE:
	define_word(`re')
C_RE:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RDE-SYSTEM

W_RH:
	define_word(`rh')
C_RH:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RHL+1-SYSTEM

W_RL:
	define_word(`rl')
C_RL:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	RHL-SYSTEM

W_CALL:
	define_word(`call')
C_CALL:
	.WORD	2+$			;Vector to code
	POP	HL			;Address of routine CALLed
	PUSH	DE			;Save register
	PUSH	BC			;Save register
	LD	A,0C3h			;Hex code for JMP
	LD	(JPCODE),A		;Save it
	LD	(JPVECT),HL		;Save jump vector
	LD	HL,(RAF)		;Get register AF
	PUSH	HL			;Onto stack
	POP	AF			;POP into AF
	LD	BC,(RBC)		;Get register BC
	LD	DE,(RDE)		;Get register DE
	LD	HL,(RHL)		;Get register HL
	LD	IX,(RIX)		;Get register IX
	LD	IY,(RIY)		;Get register IY
	CALL	JPCODE			;Call jump to code
	LD	(RIY),IY		;Save register IY
	LD	(RIX),IX		;Save register IX
	LD	(RBC),BC		;Save register BC
	LD	(RDE),DE		;Save register DE
	LD	(RHL),HL		;Save register HL
	PUSH	AF			;Save register AF
	POP	HL			;Into HL
	LD	(RAF),HL		;Into memory
	POP	BC			;Restore BC
	POP	DE			;Restore DE
	JP	NEXT			;

W_1PLUS:				;1 plus
	define_word(`1+')
C_1PLUS:
	.WORD	2+$			;Vector to code
	POP	HL			; get n
	INC	HL			; add 1
	JP	NEXTS1			; save result & NEXT

W_2PLUS:				;2 plus
	define_word(`2+')
C_2PLUS:
	.WORD	2+$			;Vector to code
	POP	HL			; get n
	INC	HL			; add 1
	INC	HL			; add 2
	JP	NEXTS1			; save result & NEXT

W_1MINUS:				;1 minus
	define_word(`1-')
C_1MINUS:
	.WORD	2+$			;Vector to code
	POP	HL			; get n
	DEC	HL			; add 1
	JP	NEXTS1			; save result & NEXT

W_2MINUS:				;2 minus
	define_word(`2-')
C_2MINUS:
	.WORD	2+$			;Vector to code
	POP	HL			; get n
	DEC	HL			; subtract 1
	DEC	HL			; subtract 2
	JP	NEXTS1			; save result & NEXT

W_HERE:					;Dictionary pointer onto stack
	define_word(`here')
C_HERE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DP			;Dictionary pointer addr on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_ALLOT:
	define_word(`allot')
C_ALLOT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DP			;Dictionary pointer addr on stack
	.WORD	C_PLUSSTORE		;Add n1 to addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_COMMA:				;Reserve 2 bytes and save n
	define_comma_word(`')
C_COMMA:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_HERE			;Next free dictionary pointer onto stack
	.WORD	C_STORE			;Store word at addr
	.WORD	C_2			;
	.WORD	C_ALLOT			;Move pointer
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CCOMMA:
	define_comma_word(`c')
C_CCOMMA:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_CSTORE		;Store byte at addr
	.WORD	C_1			;Put 1 on stack
	.WORD	C_ALLOT
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_MINUS:
	define_word(`-')
C_MINUS:
	.WORD	2+$			;Vector to code
	POP	DE			; get n1
	POP	HL			; get n2
	CALL	MINUS16			; call subtract routine
	JP	NEXTS1			; save & NEXT

MINUS16:
	LD	A,L			; gel low byte
	SUB	E			; subtract low bytes
	LD	L,A			; save low byte result
	LD	A,H			; get high byte
	SBC	A,D			; subtract high bytes
	LD	H,A			; save high byte result
	RET				;

W_.EQUALS:
	define_word(`=')
C_EQUALS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MINUS
	.WORD	C_0EQUALS		;=0
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_LESSTHAN:
	define_word(`<')
C_LESSTHAN:
	.WORD	2+$			;Vector to code
	POP	DE
	POP	HL
	LD	A,D
	XOR	H
	JP	M,J298C
	CALL	MINUS16
J298C:
	INC	H
	DEC	H
	JP	M,J2997
	LD	HL,0000
	JP	NEXTS1			;Save & NEXT
J2997:
	LD	HL,0001
	JP	NEXTS1			;Save & NEXT

W_ULESS:				;IF stack-1 < stack_top leave true flag
	define_word(`u<')
C_ULESS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_2DUP			;Dup top 2 values on stack
	.WORD	C_XOR			;Exclusive OR them
	.WORD	C_0LESS			;Less than 0
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0000-$			;000Ch
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_0LESS			;Less than 0
	.WORD	C_0EQUALS		;=0
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B0001-$			;0006h
B0000:
	.WORD	C_MINUS
	.WORD	C_0LESS			;Less than 0
B0001:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_GREATER:
	define_word(`>')
C_GREATER:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_LESSTHAN
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_ROT:					;3rd valu down to top of stack
	define_word(`rot')
C_ROT:
	.WORD	2+$			;Vector to code
	POP	DE			;Top value
	POP	HL			;Next one down
	EX	(SP),HL			;Exchange with third
	JP	NEXTS2			;Save both & NEXT

W_PICK:
	define_word(`pick')
C_PICK:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_SPFETCH		;Stack pointer onto stack
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_SPACE:
	define_word(`space')
C_SPACE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_BL			;Leaves ASCII for space on stack
	.WORD	C_EMIT			;Output CHR from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_QUERYDUP:
	define_word(`?dup')
C_QUERYDUP:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0002-$			;0004h
	.WORD	C_DUP			;Duplicate top value on stack
B0002:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_TRAVERSE:
	define_word(`traverse')
C_TRAVERSE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_SWAP			;Swap top 2 values on stack
B0054:
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	007Fh
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_CFETCH		;Get byte from addr on stack
	.WORD	C_LESSTHAN
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0054-$			;FFF0h
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_LATEST:
	define_word(`latest')
C_LATEST:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_CURRENT
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_LFA:
	define_word(`lfa')
C_LFA:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0004h
	.WORD	C_MINUS
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CFA:
	define_word(`cfa')
C_CFA:
	.WORD	2+$			;Vector to code
	POP	HL			    ; get n
	DEC	HL			    ; subtract 1
	DEC	HL			    ; subtract 2
	JP	NEXTS1			; save result & NEXT

W_NFA:
	define_word(`nfa')
C_NFA:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0005h
	.WORD	C_MINUS
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0FFFFh
	.WORD	C_TRAVERSE
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_PFA:					    ;Convert NFA to PFA
	define_word(`pfa')
C_PFA:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_1			    ;Traverse up memory
	.WORD	C_TRAVERSE		;End of name on stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0005h			;Offset to start of word code
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CSPSTORE:
	define_word(`!csp')
C_CSPSTORE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_SPFETCH		;Stack pointer onto stack
	.WORD	C_CSP			;Push check stack pointer addr
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_QERROR:
	define_word(`?error')
C_QERROR:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_0BRANCH		;Branch if no error
	.WORD	B0003-$			;0008h
	.WORD	C_ERROR
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B0004-$			;0004h
B0003:
	.WORD	C_DROP			;Drop error no.
B0004:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_QCOMP:				;Error if not in compile mode
	define_word(`?comp')
C_QCOMP:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_STATE			;Push STATE addr
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_0EQUALS		;=0
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0011h			;Error message number
	.WORD	C_QERROR		;Error if state <> 0
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_QEXEC:				;Error not if not in execute mode
	define_word(`?exec')
C_QEXEC:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_STATE			;Push STATE addr
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0012h			;Error not if not in execute mode
	.WORD	C_QERROR		;
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_QPAIRS:
	define_word(`?pairs')
C_QPAIRS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MINUS
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0013h
	.WORD	C_QERROR
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_WHATSTACK:				;Check stack pointer, error if not ok
	define_word(`?csp')
C_WHATSTACK:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_SPFETCH		;Stack pointer onto stack
	.WORD	C_CSP			;Push check stack pointer addr
	.WORD	C_FETCH			;Get check stack pointer
	.WORD	C_MINUS			;If ok then result is 0
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0014h			;Error no if not ok
	.WORD	C_QERROR		;Error if stack top -1 <> 0
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_QLOADING:
	define_word(`?loading')
C_QLOADING:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_BLK
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_0EQUALS		;=0
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0016h
	.WORD	C_QERROR
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_COMPILE:
	define_word(`compile')
C_COMPILE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_QCOMP			;Error if not in compile mode
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_DUP			;Bump return address and put back
	.WORD	C_2PLUS			;
	.WORD	C_MOVER			;
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_LEFTBRKT:				;Set STATE to execute
	define_word(`[')
C_LEFTBRKT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_STATE			;Push STATE addr
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_RIGHTBRKT:				;Set STATE to compile
	define_word(`]')
C_RIGHTBRKT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	00C0h
	.WORD	C_STATE			;Push STATE addr
	.WORD	C_STORE			;Set STATE to execute
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_SMUDGE:
	define_word(`smudge')
C_SMUDGE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LATEST		;Push top words NFA
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0020h
	.WORD	C_TOGGLE		;XOR (addr) with byte
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_HEX:
	define_word(`hex')
C_HEX:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0010h
	.WORD	C_BASE			;Put BASE addr on stack
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DECIMAL:				;Sets decimal mode
	define_word(`decimal')
C_DECIMAL:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	000Ah			;Sets decimal value
	.WORD	C_BASE			;Put BASE addr on stack
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CCODE:				;Stop compillation & terminate word
	define_word(`<;code>')
C_CCODE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_LATEST		;Push top words NFA
	.WORD	C_PFA			;Convert NFA to PFA
	.WORD	C_CFA			;Convert PFA to CFA
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_SCCODE:
	define_immediate_word(`;code')
C_SCCODE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_WHATSTACK		;Check stack pointer, error if not ok
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	C_CCODE
	.WORD	C_LEFTBRKT		;Set STATE to execute
	.WORD	C_TASK
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CREATE:
	define_word(`create')
C_CREATE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_CONSTANT
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DOES:
	define_word(`does>')
C_DOES:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_LATEST		;Push top words NFA
	.WORD	C_PFA			;Convert NFA to PFA
	.WORD	C_STORE			;Store word at addr
	.WORD	C_CCODE			;Execute following machine code

X_DOES:
	LD	HL,(RPP)		;Get return stack pointer
	DEC	HL			;Push next pointer
	LD	(HL),B			;
	DEC	HL			;
	LD	(HL),C			;
	LD	(RPP),HL
	INC	DE
	EX	DE,HL
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	INC	HL
	JP	NEXTS1			;Save & NEXT

W_COUNT:				;Convert string at addr to addr + length
	define_word(`count')
C_COUNT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DUP			;Duplicate address
	.WORD	C_1PLUS			;Add 1 (points to string start)
	.WORD	C_SWAP			;Get address back
	.WORD	C_CFETCH		;Get byte from addr on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_TYPE:					    ;Output n bytes from addr
	define_word(`type')
C_TYPE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_QUERYDUP		;Copy length if length <> 0
	.WORD	C_0BRANCH		;Branch if length = 0
	.WORD	B0005-$			;0018h
	.WORD	C_OVER			;Copy address to stack top
	.WORD	C_PLUS			;Add to length
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_LDO			;Put start & end loop values on RPP
B004F:
	.WORD	C_I			    ;Copy LOOP index to data stack
	.WORD	C_CFETCH		;Get byte from string
	.WORD	C_EMIT			;Output CHR from stack
	.WORD	C_LLOOP			;Increment loop & branch if not done
	.WORD	B004F-$			;FFF8h
	.WORD	C_BRANCH		;Done so branch to next
	.WORD	B0006-$			;0004h
B0005:
	.WORD	C_DROP			;Drop string address
B0006:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_TRAILING:
	define_word(`-trailing')
C_TRAILING:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_LDO			;Put start & end loop values on RPP
B0009:
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_1			;Put 1 on stack
	.WORD	C_MINUS
	.WORD	C_CFETCH		;Get byte from addr on stack
	.WORD	C_BL			;Leaves ASCII for space on stack
	.WORD	C_MINUS
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0007-$			;0008h
	.WORD	C_LEAVE			;Quit loop by making index = limit
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B0008-$			;0006h
B0007:
	.WORD	C_1			;Put 1 on stack
	.WORD	C_MINUS
B0008:
	.WORD	C_LLOOP			;Increment loop & branch if not done
	.WORD	B0009-$			;FFE0h
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CQUOTE:				;Output following string
	.BYTE	84h
	.ascii  "<."
	.byte   '"', '>' + 80h		; M4 problem
	.WORD	W_TRAILING
C_CQUOTE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_RFETCH		;Copy return stack top to data stack
	.WORD	C_COUNT			;Convert string at addr to addr + length
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_1PLUS			;1 plus
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_PLUS			;Add length of string +1
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_TYPE			;Output n bytes from addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_QUOTE:				;Accept following text
	.set last_word_address, .
	.BYTE	0C2h, '.', '"'+80h		; M4 problem
	.WORD	W_CQUOTE
C_QUOTE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0022h
	.WORD	C_STATE			;Push STATE addr
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B000A-$			;0012h
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	C_CQUOTE		;
	.WORD	C_WORD
	.WORD	C_CFETCH		;Get byte from addr on stack
	.WORD	C_1PLUS			;1 plus
	.WORD	C_ALLOT
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B000B-$			;0008h
B000A:
	.WORD	C_WORD
	.WORD	C_COUNT			;Convert string at addr to addr + length
	.WORD	C_TYPE			;Output n bytes from addr
B000B:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_EXPECT:
	define_word(`expect')
C_EXPECT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_OVER			;Copy buffer start addr
	.WORD	C_PLUS			;Add to length to give start,end
	.WORD	C_OVER			;Copy start
	.WORD	C_LDO			;Put start & end loop values on RPP
B0012:
	.WORD	C_KEY			;Wait for key, value on stack
	.WORD	C_DUP			;Duplicate key value
	.WORD	C_LIT			;Push backspace addr
	.WORD	BACKSPACE		;
	.WORD	C_FETCH			;Get backspace value
	.WORD	C_EQUALS		;Was it backspace ?
	.WORD	C_0BRANCH		;If not then jump
	.WORD	B000C-$			;002Ah
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_I			;Copy LOOP index to data stack
	.WORD	C_EQUALS
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_2
	.WORD	C_MINUS
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B000D-$			;000Ah
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0007h
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B000E-$			;0006h
B000D:
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0008h
B000E:
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B000F-$			;0028h
B000C:
	.WORD	C_DUP			;Duplicate key value
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	000Dh			;CR
	.WORD	C_EQUALS		;Was it cariage return
	.WORD	C_0BRANCH		;If not then jump
	.WORD	B0010-$			;000Eh
	.WORD	C_LEAVE			;Quit loop by making index = limit
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_BL			;Leaves ASCII for space on stack
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B0011-$			;0004h
B0010:
	.WORD	C_DUP			;Duplicate key value
B0011:
	.WORD	C_I			;Copy LOOP index to data stack
	.WORD	C_CSTORE		;Store byte at addr
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_I			;Copy LOOP index to data stack
	.WORD	C_1PLUS			;1 plus
	.WORD	C_STORE			;Store word at addr
B000F:
	.WORD	C_EMIT			;Output CHR from stack
	.WORD	C_LLOOP			;Increment loop & branch if not done
	.WORD	B0012-$			;FF9Eh
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_QUERY:
	define_word(`query')
C_QUERY:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_TIB			;Put TIB addr on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0050h			;Max line length 50h
	.WORD	C_EXPECT		;Get line
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_TOIN			;Current input buffer offset
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_NULL:
	.set last_word_address, .
	.BYTE	0C1h,80h		; M4 problem ?
	.WORD	W_QUERY
C_NULL:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_BLK
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0013-$			;002Ah
	.WORD	C_1			;Put 1 on stack
	.WORD	C_BLK
	.WORD	C_PLUSSTORE		;Add n1 to addr
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_TOIN			;Current input buffer offset
	.WORD	C_STORE			;Store word at addr
	.WORD	C_BLK
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_BSCR			;Number of buffers per block on stack
	.WORD	C_1			;Put 1 on stack
	.WORD	C_MINUS
	.WORD	C_AND			;AND
	.WORD	C_0EQUALS		;=0
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0014-$			;0008h
	.WORD	C_QEXEC			;Error not if not in execute mode
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_DROP			;Drop top value from stack
B0014:
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B0015-$			;0006h
B0013:
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_DROP			;Drop top value from stack
B0015:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_FILL:					;Fill with byte n bytes from addr
	define_word(`fill')
C_FILL:
	.WORD	2+$			;Vector to code
	LD	L,C			;Save BC for now
	LD	H,B			;
	POP	DE			; get byte
	POP	BC			; get n
	EX	(SP),HL			; get addr and save BC
	EX	DE,HL			;
NEXT_BYTE:
	LD	A,B			;Test count
	OR	C			;
	JR	Z,NO_COUNT		;If 0 we're done
	LD	A,L			;Byte into A
	LD	(DE),A			;Save byte
	INC	DE			;Next addr
	DEC	BC			;Decr count
	JR	NEXT_BYTE		;Loop
NO_COUNT:
	POP	BC			;Get BC back
	JP	NEXT

W_ERASE:				;Fill addr & length from stack with 0
	define_word(`erase')
C_ERASE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_FILL			;Fill with byte n bytes from addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_BLANKS:				;Fill addr & length from stack with [SP]
	define_word(`blanks')
C_BLANKS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_BL			;Leaves ASCII for space on stack
	.WORD	C_FILL			;Fill with byte n bytes from addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_HOLD:
	define_word(`hold')
C_HOLD:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0FFFFh
	.WORD	C_HLD
	.WORD	C_PLUSSTORE		;Add n1 to addr
	.WORD	C_HLD
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_CSTORE		;Store byte at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_PAD:
	define_word(`pad')
C_PAD:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0044h
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_WORD:
	define_word(`word')
C_WORD:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_BLK
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0016-$			;000Ch
	.WORD	C_BLK
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_BLOCK
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B0017-$			;0006h
B0016:
	.WORD	C_TIB
	.WORD	C_FETCH			;Get word from addr on stack
B0017:
	.WORD	C_TOIN			;Current input buffer offset
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_ENCLOSE
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0022h
	.WORD	C_BLANKS
	.WORD	C_TOIN			;Current input buffer offset
	.WORD	C_PLUSSTORE		;Add n1 to addr
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_MINUS
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_RFETCH		;Return stack top to data stack
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_CSTORE		;Store byte at addr
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_1PLUS			;1 plus
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_CMOVE			;Move block
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CONVERT:
	define_word(`convert')
C_CONVERT:
	.WORD	E_COLON			;Interpret following word sequence
B001A:
	.WORD	C_1PLUS			;1 plus
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_CFETCH		;Get byte from addr on stack
	.WORD	C_BASE			;Put BASE addr on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_DIGIT			;Convert digit n2 using base n1
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0018-$			;002Ch
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_BASE			;Put BASE addr on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_USTAR
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_ROT			;3rd value down to top of stack
	.WORD	C_BASE			;Put BASE addr on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_USTAR
	.WORD	C_DPLUS
	.WORD	C_DPL
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_1PLUS			;1 plus
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0019-$			;0008h
	.WORD	C_1			;Put 1 on stack
	.WORD	C_DPL
	.WORD	C_PLUSSTORE		;Add n1 to addr
B0019:
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B001A-$			;FFC6h
B0018:
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_NUMBER:
	define_word(`number')
C_NUMBER:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_ROT			;3rd value down to top of stack
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_1PLUS			;1 plus
	.WORD	C_CFETCH		;Get byte from addr on stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	002Dh			;'-'
	.WORD	C_EQUALS		;Is first chr = '-'
	.WORD	C_DUP			;Duplicate negative flag
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0FFFFh			; -1
B001C:
	.WORD	C_DPL
	.WORD	C_STORE			;Store word at addr
	.WORD	C_CONVERT
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_CFETCH		;Get byte from addr on stack
	.WORD	C_BL			;Leaves ASCII for space on stack
	.WORD	C_MINUS
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B001B-$			;0016h
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_CFETCH		;Get byte from addr on stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	002Eh			;'.'
	.WORD	C_MINUS
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_QERROR
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B001C-$			;FFDCh
B001B:
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B001D-$			;0004h
	.WORD	C_DNEGATE
B001D:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_MFIND:
	define_word(`-find')
C_MFIND:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_BL			;Leaves ASCII for space on stack
	.WORD	C_WORD
	.WORD	C_CONTEXT
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_FIND			;Find word & return vector,byte & flag
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_0EQUALS		;=0
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B001E-$			;000Ah
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_LATEST		;Push top words NFA
	.WORD	C_FIND			;Find word & return vector,byte & flag
B001E:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CABORT:
	define_word(`<abort>')
C_CABORT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ABORT
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_ERROR:
	define_word(`error')
C_ERROR:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_WARNING		;Put WARNING addr on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_0LESS			;Less than 0 leaves true
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B001F-$			;0004h
	.WORD	C_CABORT
B001F:
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_COUNT			;Convert string at addr to addr + length
	.WORD	C_TYPE			;Output n bytes from addr
	.WORD	C_CQUOTE		;Output following string
	.BYTE	S_END7-S_START7
S_START7:
	.ascii	"? "		;
S_END7:
	.WORD	C_MESSAGE		;Output message
	.WORD	C_SPSTORE		;Set initial stack pointer value
	.WORD	C_BLK
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_QUERYDUP
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0020-$			;0008h
	.WORD	C_TOIN			;Current input buffer offset
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_SWAP			;Swap top 2 values on stack
B0020:
	.WORD	C_QUIT

W_ID:					;Print definition name from name field addr
	define_word(`id.')
C_ID:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_COUNT			;Convert string at addr to addr + length
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	001Fh			;Max length is 1Fh
	.WORD	C_AND			;AND lenght with 1Fh
	.WORD	C_TYPE			;Output n bytes from addr
	.WORD	C_SPACE			;Output space
	.WORD	C_STOP			;Pop BC from return stack (=next)

C_XXX1:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MFIND			;Find name returns PFA,length,true or false
	.WORD	C_0BRANCH		;Branch if name not found
	.WORD	B0021-$			;0010h
	.WORD	C_DROP			;Drop length
	.WORD	C_NFA			;Convert PFA to NFA
	.WORD	C_ID			;Print definition name from name field addr
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0004h			;Message 4, name defined twice
	.WORD	C_MESSAGE		;Output message
	.WORD	C_SPACE			;Output space
B0021:
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_CFETCH		;Get byte from addr on stack
	.WORD	C_WIDTH
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_MIN
	.WORD	C_1PLUS			;1 plus
	.WORD	C_ALLOT			;Which ever is smallest width or namelength
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	00A0h
	.WORD	C_TOGGLE		;XOR (addr) with byte
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_1			;Put 1 on stack
	.WORD	C_MINUS
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0080h
	.WORD	C_TOGGLE		;XOR (addr) with byte
	.WORD	C_LATEST		;Push top words NFA
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_CURRENT
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_STORE			;Store word at addr
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_2PLUS			;2 plus
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CCOMPILE:
	define_word(`[compile]')
C_CCOMPILE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MFIND
	.WORD	C_0EQUALS		;=0
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_QERROR
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_CFA			;Convert PFA to CFA
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_LITERAL:
	define_immediate_word(`literal')
C_LITERAL:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_STATE			;Push STATE addr
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0022-$			;0008h
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	C_COMMA			;Reserve 2 bytes and save n
B0022:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DLITERAL:
	define_immediate_word(`dliteral')
C_DLITERAL:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_STATE			;Push STATE addr
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0023-$			;0008h
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_LITERAL
	.WORD	C_LITERAL
B0023:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_QSTACK:
	define_word(`?stack')
C_QSTACK:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_SPFETCH		;Stack pointer onto stack
	.WORD	C_S0			;Push S0 (initial data stack pointer)
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_ULESS			;IF stack-1 < stack_top leave true flag
	.WORD	C_1			;Put 1 on stack
	.WORD	C_QERROR
	.WORD	C_SPFETCH		;Stack pointer onto stack
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0080h
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_ULESS			;IF stack-1 < stack_top leave true flag
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0007h
	.WORD	C_QERROR
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_INTERPRET:
	define_word(`interpret')
C_INTERPRET:
	.WORD	E_COLON			;Interpret following word sequence
B002A:
	.WORD	C_MFIND			;Find name returns PFA,length,true or false
	.WORD	C_0BRANCH		;Branch if name not found
	.WORD	NO_NAME-$		;
	.WORD	C_STATE			;STATE addr on stack
	.WORD	C_FETCH			;Get STATE
	.WORD	C_LESSTHAN		;Is it quit compile word ?
	.WORD	C_0BRANCH		;If so then branch
	.WORD	B0025-$			;
	.WORD	C_CFA			;Convert PFA to CFA
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B0026-$			;
B0025:
	.WORD	C_CFA			;Convert PFA to CFA
	.WORD	C_EXECUTE		;Jump to address on stack
B0026:
	.WORD	C_QSTACK		;Error message if stack underflow
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B0027-$			;
NO_NAME:
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_NUMBER		;Convert string at addr to double
	.WORD	C_DPL			;
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_1PLUS			;1 plus
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0028-$			;
	.WORD	C_DLITERAL
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B0029-$			;
B0028:
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_LITERAL
B0029:
	.WORD	C_QSTACK		;Error message if stack underflow
B0027:
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B002A-$			;FFC2h

W_IMMEDIATE:
	define_word(`immediate')
C_IMMEDIATE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LATEST		;Push top words NFA
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0040h
	.WORD	C_TOGGLE		;XOR (addr) with byte
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_VOCABULARY:
	define_word(`vocabulary')
C_VOCABULARY:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_CREATE
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0A081h
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_CURRENT
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_CFA			;Convert PFA to CFA
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_VOC_LINK
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_VOC_LINK
	.WORD	C_STORE			;Store word at addr
	.WORD	C_DOES
	.WORD	C_2PLUS			;2 plus
	.WORD	C_CONTEXT
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

C_LINK:
	.WORD	C_2PLUS			;2 plus
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_CONTEXT
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_FORTH:
	define_immediate_word(`forth')
C_FORTH:
	.WORD	X_DOES
	.WORD	C_LINK

	.BYTE	81h,' '+80h		; XXX $20 or $A0 ?
	.WORD	FLAST+2
E_FORTH:
	.WORD	0000h

W_DEFINITIONS:				;Set CURRENT as CONTEXT vocabulary
	define_word(`definitions')
C_DEFINITIONS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_CONTEXT		;Get CONTEXT addr
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_CURRENT		;Get CURRENT addr
	.WORD	C_STORE			;Set CURRENT as the context vocabulary
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_OPENBRKT:
	.set this_word, .
    .byte 0C0h+1
    .byte '`('' + 80h	; M4 problem
	.word last_word_address
	.set last_word_address, this_word
C_OPENBRKT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0029h
	.WORD	C_WORD
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

;		This it the last thing ever executed and is the interpreter
;		outer loop. This NEVER quits.

W_QUIT:
	define_word(`quit')
C_QUIT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_BLK			;Get current BLK pointer
	.WORD	C_STORE			;Set BLK to 0
	.WORD	C_LEFTBRKT		;Set STATE to execute
B002C:
	.WORD	C_RPSTORE		;Set initial return stack pointer
	.WORD	C_CR			;Output [CR][LF]
	.WORD	C_QUERY			;Get string from input, ends in CR
	.WORD	C_INTERPRET		;Interpret input stream
	.WORD	C_STATE			;Push STATE addr
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_0EQUALS		;=0
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	S_END8-$		;0007h
	.WORD	C_CQUOTE		;Output following string
	.BYTE	S_END8-S_START8
S_START8:
	.ascii	"OK"
S_END8:
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B002C-$			;FFE7h

W_ABORT:
	define_word(`abort')
C_ABORT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_UABORT		;Put UABORT on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_EXECUTE		;Jump to address on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

CF_UABORT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_SPSTORE		;Set initial stack pointer value
	.WORD	C_DECIMAL		;Sets decimal mode
	.WORD	C_QSTACK		;Error message if stack underflow
	.WORD	C_CR			;Output [CR][LF]
	.WORD	C_CQUOTE		;Output following string
	.BYTE	S_END1-S_START1		;String length
S_START1:
	.ascii	"* Z80 FORTH *"
S_END1:
	.WORD	C_FORTH
	.WORD	C_DEFINITIONS		;Set CURRENT as CONTEXT vocabulary
	.WORD	C_QUIT

W_WARM:
	define_word(`warm')
C_WARM:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	WORD1			;Start of detault table
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	S0			;S0 addr
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	START_TABLE-WORD1	;Table length
	.WORD	C_CMOVE			;Move block
	.WORD	C_ABORT

X_COLD:

	LD	HL,START_TABLE	        ;Copy table to ram
	LD	DE,FLAST		        ;Where the table's going
	LD	BC,NEXTS2-START_TABLE	;Bytes to copy
	LDIR				   ;
	LD	HL,W_TASK		   ; Copy last word to ram -- need to update when creating a NEW WORD
	LD	DE,VOCAB_BASE	   ; Where it's going
	LD	BC,W_TASKEND-W_TASK ;Bytes to copy
	LDIR				   ;
	LD	BC,FIRSTWORD	   ;BC to first forth word
	LD	HL,(WORD1)		;Get stack pointer
	LD	SP,HL			;Set it
	JP	NEXT

FIRSTWORD:
	.WORD	C_COLD

W_COLD:
	define_word(`cold')
	.WORD	X_COLD
C_COLD:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_EBUFFERS		;Clear pseudo disk buffer
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_OFFSET		;Put disk block offset on stack
	.WORD	C_STORE			;Clear disk block offset
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	WORD1			;Start of default table
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	S0			;S0 addr
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	START_TABLE-WORD1	;Block length on stack (0010h)
	.WORD	C_CMOVE			;Move block
	.WORD	C_ABORT

W_SINGTODUB:				;Change single number to double
	define_word(`s->d')
C_SINGTODUB:
	.WORD	2+$			;Vector to code
	POP	DE			;Get number
	LD	HL,0000h		;Assume +ve extend
	LD	A,D			;Check sign
	AND	80h			;
	JR	Z,IS_POS		;Really +ve so jump
	DEC	HL			;Make -ve extension
IS_POS:
	JP	NEXTS2			;Save both & NEXT

W_PLUSMINUS:
	define_word(`+-')
C_PLUSMINUS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_0LESS			;Less than 0
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B002D-$			;0004h
	.WORD	C_NEGATE		;Form 2s complement of n
B002D:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DPLUSMINUS:				;Add sign of n to double
	define_word(`d+-')
C_DPLUSMINUS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_0LESS			;Less than 0
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B002E-$			;0004h
	.WORD	C_DNEGATE
B002E:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_ABS:
	define_word(`abs')
C_ABS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_PLUSMINUS
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DABS:
	define_word(`dabs')
C_DABS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_DPLUSMINUS		;Add sign of n to double
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_MIN:
	define_word(`min')
C_MIN:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_2DUP			;Dup top 2 values on stack
	.WORD	C_GREATER
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B002F-$			;0004h
	.WORD	C_SWAP			;Swap top 2 values on stack
B002F:
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_MAX:
	define_word(`max')
C_MAX:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_2DUP			;Dup top 2 values on stack
	.WORD	C_LESSTHAN
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0030-$			;0004h
	.WORD	C_SWAP			;Swap top 2 values on stack
B0030:
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_MTIMES:
	define_word(`m*')
C_MTIMES:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_2DUP			;Dup top 2 values on stack
	.WORD	C_XOR			;Works out sign of result
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_ABS
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_ABS
	.WORD	C_USTAR
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_DPLUSMINUS		;Add sign of n to double
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_MDIV:
	define_word(`m/')
C_MDIV:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_DABS
	.WORD	C_RFETCH		;Return stack top to data stack
	.WORD	C_ABS
	.WORD	C_UMOD			;Unsigned divide & MOD
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_RFETCH		;Return stack top to data stack
	.WORD	C_XOR			;XOR
	.WORD	C_PLUSMINUS
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_PLUSMINUS
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_TIMES:
	define_word(`*')
C_TIMES:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MTIMES
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DIVMOD:
	define_word(`/mod')
C_DIVMOD:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_SINGTODUB		;Change single number to double
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_MDIV
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DIV:
	define_word(`/')
C_DIV:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DIVMOD
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_MOD:
	define_word(`mod')
C_MOD:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DIVMOD
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_TIMESDIVMOD:
	define_word(`*/mod')
C_TIMESDIVMOD:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_MTIMES
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_MDIV
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_TIMESDIV:
	define_word(`*/')
C_TIMESDIV:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_TIMESDIVMOD
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_MDIVMOD:
	define_word(`m/mod')
C_MDIVMOD:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_RFETCH		;Return stack top to data stack
	.WORD	C_UMOD			;Unsigned divide & MOD
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_UMOD			;Unsigned divide & MOD
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CLINE:
	define_word(`<line>')
C_CLINE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_CL			;Put characters/line on stack
	.WORD	C_BBUF			;Put bytes per block on stack
	.WORD	C_TIMESDIVMOD
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_BSCR			;Number of buffers per block on stack
	.WORD	C_TIMES
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_BLOCK
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_CL			;Put characters/line on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DOTLINE:
	define_word(`.line')
C_DOTLINE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_CLINE
	.WORD	C_TRAILING
	.WORD	C_TYPE			;Output n bytes from addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_MESSAGE:
	define_word(`message')
C_MESSAGE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_WARNING		;Put WARNING addr on stack
	.WORD	C_FETCH			;Get WARNING value
	.WORD	C_0BRANCH		;If WARNING = 0 output MSG # n
	.WORD	B0031-$			;001Eh
	.WORD	C_QUERYDUP
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0032-$			;0014h
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0004h
	.WORD	C_OFFSET		;Put disk block offset on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_BSCR			;Number of buffers per block on stack
	.WORD	C_DIV
	.WORD	C_MINUS
	.WORD	C_DOTLINE		;Output line from screen
	.WORD	C_SPACE			;Output space
B0032:
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B0033-$			;000Dh
B0031:
	.WORD	C_CQUOTE		;Output following string
		.BYTE	S_END2-S_START2
S_START2:
	.ascii	"MSG # "
S_END2:
	.WORD	C_DOT
B0033:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_PORTIN:				;Fetch data from port
	define_word(`p@')
C_PORTIN:
	.WORD	2+$			;Vector to code
	POP	DE			;Get port addr
	LD	HL,PAT+1		;Save in port in code
	LD	(HL),E			;
	CALL	PAT			;Call port in routine
	LD	L,A			;Save result
	LD	H,00h			;
	JP	NEXTS1			;Save & NEXT

W_PORTOUT:				;Save data to port
	define_word(`p!')
C_PORTOUT:
	.WORD	2+$			;Vector to code
	POP	DE			;Get port addr
	LD	HL,PST+1		;Save in port out code
	LD	(HL),E			;
	POP	HL			;
	LD	A,L			;Byte to A
	CALL	PST			;Call port out routine
	JP	NEXT

W_USE:
	define_word(`use')
C_USE:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	USE-SYSTEM

W_PREV:
	define_word(`prev')
C_PREV:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	PREV-SYSTEM

W_PLUSBUF:
	define_word(`+buf')
C_PLUSBUF:
	.WORD	NEXT

W_UPDATE:
	define_word(`update')
C_UPDATE:
	.WORD	NEXT

W_EBUFFERS:				;Clear pseudo disk buffer
	define_word(`empty-buffers')
C_EBUFFERS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_FIRST			;Start of pseudo disk onto stack
	.WORD	C_LIMIT			;End of pseudo disk onto stack
	.WORD	C_OVER			;Start to top of stack
	.WORD	C_MINUS			;Work out buffer length
	.WORD	C_ERASE			;Fill addr & length from stack with 0
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_BUFFER:
	define_word(`buffer')
C_BUFFER:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_BLOCK
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_BLOCK:				;Put address of block n (+ offset) on stack
	define_word(`block')
C_BLOCK:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	MAX_DISK_BLOCKS ;Max number of blocks
	.WORD	C_MOD			;MOD to max number
	.WORD	C_OFFSET		;Put address of disk block offset on stack
	.WORD	C_FETCH			;Get disk block offset
	.WORD	C_PLUS			;Add offset to block #
	.WORD	C_BBUF			;Put bytes per block on stack
	.WORD	C_TIMES			;Bytes times block number
	.WORD	C_FIRST			;Put address of first block on stack
	.WORD	C_PLUS			;Add address of first to byte offset
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_RW:
	define_word(`r/w')
C_RW:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_URW			;
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_EXECUTE		;Jump to address on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)
CF_URW:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_FLUSH:
	define_word(`flush')
C_FLUSH:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DUMP:
	define_word(`dump')
C_DUMP:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_LDO			;Put start & end loop values on RPP
B0051:
	.WORD	C_CR			;Output [CR][LF]
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0005h
	.WORD	C_DDOTR
	.WORD	C_SPACE			;Output space
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0004h
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_LDO			;Put start & end loop values on RPP
B0050:
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_CFETCH		;Get byte from addr on stack
	.WORD	C_3
	.WORD	C_DOTR
	.WORD	C_1PLUS			;1 plus
	.WORD	C_LLOOP			;Increment loop & branch if not done
	.WORD	B0050-$			;FFF4h
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_PLOOP			;Loop + stack & branch if not done
	.WORD	B0051-$			;FFD4h
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_CR			;Output [CR][LF]
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_LOAD:
	define_word(`load')
C_LOAD:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_BLK			;Get current block number (0 = keyboard)
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_MOVER			;Save it for now
	.WORD	C_TOIN			;Current input buffer offset
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_MOVER			;Save it for now
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_TOIN			;Current input buffer offset
	.WORD	C_STORE			;Set to zero
	.WORD	C_BSCR			;Number of buffers per block on stack
	.WORD	C_TIMES			;Multiply block to load by buffers/block
	.WORD	C_BLK			;Get BLK pointer
	.WORD	C_STORE			;Make load block current input stream
	.WORD	C_INTERPRET		;Interpret input stream
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_TOIN			;Current input buffer offset
	.WORD	C_STORE			;Store word at addr
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_BLK			;Current block
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_NEXTSCREEN:
	define_immediate_word(`-->')
C_NEXTSCREEN:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_QLOADING
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_TOIN			;Current input buffer offset
	.WORD	C_STORE			;Store word at addr
	.WORD	C_BSCR			;Number of buffers per block on stack
	.WORD	C_BLK
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_MOD
	.WORD	C_MINUS
	.WORD	C_BLK
	.WORD	C_PLUSSTORE		;Add n1 to addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_TICK:
	.set last_word_address, .
	.BYTE	81h,27h+80h ; XXX
	.WORD	W_NEXTSCREEN
C_TICK:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MFIND			;Find name returns PFA,length,true or false
	.WORD	C_0EQUALS		;=0
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_QERROR
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_LITERAL
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_FORGET:
	define_word(`forget')
C_FORGET:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_CURRENT
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_CONTEXT
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_MINUS
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0018h
	.WORD	C_QERROR
	.WORD	C_TICK
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_FENCE
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_LESSTHAN
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0015h
	.WORD	C_QERROR
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_NFA			;Convert PFA to NFA
	.WORD	C_DP			;Dictionary pointer addr on stack
	.WORD	C_STORE			;Store word at addr
	.WORD	C_LFA
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_CONTEXT
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_BACK:
	define_word(`back')
C_BACK:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_MINUS
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_BEGIN:
	define_immediate_word(`begin')
C_BEGIN:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_QCOMP			;Error if not in compile mode
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_1			;Put 1 on stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_ENDIF:
	define_immediate_word(`endif')
C_ENDIF:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_QCOMP			;Error if not in compile mode
	.WORD	C_2
	.WORD	C_QPAIRS
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_MINUS
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_THEN:
	define_immediate_word(`then')
C_THEN:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ENDIF
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DO:
	define_immediate_word(`do')
C_DO:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	C_LDO			;Put start & end loop values on RPP
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_3
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_LOOP:
	define_immediate_word(`loop')
C_LOOP:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_3
	.WORD	C_QPAIRS
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	C_LLOOP			;Increment loop & branch if not done
	.WORD	C_BACK
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_PLUSLOOP:
	define_immediate_word(`+loop')
C_PLUSLOOP:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_3
	.WORD	C_QPAIRS
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	C_PLOOP			;Loop + stack & branch if not done
	.WORD	C_BACK
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_UNTIL:
	define_immediate_word(`until')
C_UNTIL:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_1			;Put 1 on stack
	.WORD	C_QPAIRS
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	C_BACK
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_END:
	define_immediate_word(`end')
C_END:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_UNTIL
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_AGAIN:
	define_immediate_word(`again')
C_AGAIN:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_1			;Put 1 on stack
	.WORD	C_QPAIRS
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	C_BACK
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_REPEAT:
	define_immediate_word(`repeat')
C_REPEAT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_AGAIN
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_2
	.WORD	C_MINUS
	.WORD	C_ENDIF
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_IF:
	define_immediate_word(`if')
C_IF:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_2
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_ELSE:
	define_immediate_word(`else')
C_ELSE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_2
	.WORD	C_QPAIRS
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	C_HERE			;Dictionary pointer onto stack
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_2
	.WORD	C_ENDIF
	.WORD	C_2
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_WHILE:
	define_immediate_word(`while')
C_WHILE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_IF
	.WORD	C_2PLUS			;2 plus
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_SPACES:
	define_word(`spaces')
C_SPACES:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_MAX
	.WORD	C_QUERYDUP
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0034-$			;000Ch
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_LDO			;Put start & end loop values on RPP
B0035:
	.WORD	C_SPACE			;Output space
	.WORD	C_LLOOP			;Increment loop & branch if not done
	.WORD	B0035-$			;FFFCh
B0034:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_LESSHARP:
	define_word(`<#')
C_LESSHARP:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_PAD			;Save intermediate string address
	.WORD	C_HLD
	.WORD	C_STORE			;Store word at addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_SHARPGT:
	define_word(`#>')
C_SHARPGT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_HLD
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_PAD			;Save intermediate string address
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_MINUS
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_SIGN:
	define_word(`sign')
C_SIGN:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ROT			;3rd valu down to top of stack
	.WORD	C_0LESS			;Less than 0
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0036-$			;0008h
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	002Dh
	.WORD	C_HOLD
B0036:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_SHARP:
	define_word(`#')
C_SHARP:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_BASE			;Put BASE addr on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_MDIVMOD
	.WORD	C_ROT			;3rd valu down to top of stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0009h
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_LESSTHAN
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0037-$			;0008h
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0007h
	.WORD	C_PLUS			;n1 + n2
B0037:
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0030h
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_HOLD
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_SHARPS:
	define_word(`#s')
C_SHARPS:
	.WORD	E_COLON			;Interpret following word sequence
B0038:
	.WORD	C_SHARP
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_OR			;OR
	.WORD	C_0EQUALS		;=0
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0038-$			;FFF4h
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DDOTR:
	define_word(`d.r')
C_DDOTR:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_DABS
	.WORD	C_LESSHARP
	.WORD	C_SHARPS
	.WORD	C_SIGN
	.WORD	C_SHARPGT
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_OVER			;Copy 2nd down to top of stack
	.WORD	C_MINUS
	.WORD	C_SPACES
	.WORD	C_TYPE			;Output n bytes from addr
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DOTR:
	define_word(`.r')
C_DOTR:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_SINGTODUB		;Change single number to double
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_DDOTR
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DDOT:
	define_word(`d.')
C_DDOT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_DDOTR
	.WORD	C_SPACE			;Output space
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DOT:
	define_word(`.')
C_DOT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_SINGTODUB		;Change single number to double
	.WORD	C_DDOT
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_QUESTION:
	define_word(`?')
C_QUESTION:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_DOT
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_UDOT:					;Output as unsigned value
	define_word(`u.')
C_UDOT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_DDOT			;Output double value
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_VLIST:
	define_word(`words')
C_VLIST:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_CONTEXT		;Leave vocab pointer on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_CR			;Output [CR][LF]
B0039:
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_PFA			;Convert NFA to PFA
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_ID			;Print definition name from name field addr
	.WORD	C_LFA			;Convert param addr to link addr
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_0EQUALS		;=0
	.WORD	C_TERMINAL		;Check for break key
	.WORD	C_OR			;OR
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0039-$			;FFE2h
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_CR			;Output [CR][LF]
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_LIST:
	define_word(`list')
C_LIST:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_BASE			;Put BASE addr on stack
	.WORD	C_FETCH			;Put current base on stack
	.WORD	C_SWAP			;Get number of list screen to top
	.WORD	C_DECIMAL		;Sets decimal mode
	.WORD	C_CR			;Output [CR][LF]
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_SCR			;Set most recently listed
	.WORD	C_STORE			;Store word at addr
	.WORD	C_CQUOTE		;Output following string
	.BYTE	S_END3-S_START3
S_START3:
	.ascii	"SCR # "
S_END3:
	.WORD	C_DOT			;Output the screen number
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0010h			;16 lines to do
	.WORD	C_ZERO			;From 0 to 15
	.WORD	C_LDO			;Put start & end loop values on RPP
DO_LINE:
	.WORD	C_CR			;Output [CR][LF]
	.WORD	C_I		    	;Line number onto data stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0003h			;Fromat right justified 3 characters
	.WORD	C_DOTR			;Output formatted
	.WORD	C_SPACE			;Output space
	.WORD	C_I		     	;Line number onto data stack
	.WORD	C_SCR			;Get screen number
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_DOTLINE		;Output line from screen
	.WORD	C_TERMINAL		;Check for break key
	.WORD	C_0BRANCH		;Jump if no break key
	.WORD	NO_BRK-$		;
	.WORD	C_LEAVE			;Else set loop index to limit (quit loop)
NO_BRK:
	.WORD	C_LLOOP			;Increment loop & branch if not done
	.WORD	DO_LINE-$		;
	.WORD	C_CR			;Output [CR][LF]
	.WORD	C_BASE			;Put BASE addr on stack
	.WORD	C_STORE			;Restore original base
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_INDEX:
	define_word(`index')
C_INDEX:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_1PLUS			;1 plus
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_LDO			;Put start & end loop values on RPP
B003D:
	.WORD	C_CR			;Output [CR][LF]
	.WORD	C_I			;Copy LOOP index to data stack
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0003h
	.WORD	C_DOTR
	.WORD	C_SPACE			;Output space
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_I			;Copy LOOP index to data stack
	.WORD	C_DOTLINE		;Output line from screen
	.WORD	C_TERMINAL		;Check for break key
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B003C-$			;0004h
	.WORD	C_LEAVE			;Quit loop by making index = limit
B003C:
	.WORD	C_LLOOP			;Increment loop & branch if not done
	.WORD	B003D-$			;FFE4h
	.WORD	C_CR			;Output [CR][LF]
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_INT:
	define_immediate_word(`;int')
C_INT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_WHATSTACK		;Check stack pointer, error if not ok
	.WORD	C_COMPILE		;Compile next word into dictionary
	.WORD	X_INT
	.WORD	C_LEFTBRKT		;Set STATE to execute
	.WORD	C_SMUDGE
	.WORD	C_STOP			;Pop BC from return stack (=next)

X_INT:
	.WORD	2+$			;Vector to code
	LD	HL,INTFLAG
	RES	6,(HL)
	EI
	JP	X_STOP

W_INTFLAG:
	define_word(`intflag')
C_INTFLAG:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	INTFLAG-SYSTEM

W_INTVECT:
	define_word(`intvect')
C_INTVECT:
	.WORD	X_USER			;Put next word on stack then do next
	.WORD	INTVECT-SYSTEM

W_CPU:
	define_word(`.cpu')
C_CPU:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_CQUOTE		;Output following string
	.BYTE	S_END4-S_START4
S_START4:
	.ascii	"Z80 "
S_END4:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_2SWAP:
	define_word(`2swap')
C_2SWAP:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ROT			;3rd valu down to top of stack
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_ROT			;3rd valu down to top of stack
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_2OVER:
	define_word(`2over')
C_2OVER:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_2DUP			;Dup top 2 values on stack
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_2SWAP
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_EXIT:
	define_word(`exit')
C_EXIT:
	.WORD	X_STOP

W_J:					;Push outer loop value on stack
	define_word(`j')
C_J:
	.WORD	2+$			;Vector to code
	LD	HL,(RPP)		;Get return stack pointer
	INC	HL			;Skip inner loop values
	INC	HL			;
	INC	HL			;
	INC	HL			;
	JP	X_I2

W_ROLL:
	define_word(`roll')
C_ROLL:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_GREATER
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B003E-$			;002Ch
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_MOVER			;Move value from data to return stack
	.WORD	C_PICK
	.WORD	C_RMOVE			;Move word from return to data stack
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_LDO			;Put start & end loop values on RPP
B003F:
	.WORD	C_SPFETCH		;Stack pointer onto stack
	.WORD	C_I			;Copy LOOP index to data stack
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_PLUS			;n1 + n2
	.WORD	C_DUP			;Duplicate top value on stack
	.WORD	C_2MINUS		;2 minus
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_SWAP			;Swap top 2 values on stack
	.WORD	C_STORE			;Store word at addr
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0FFFFh
	.WORD	C_PLOOP			;Loop + stack & branch if not done
	.WORD	B003F-$			;FFE6h
B003E:
	.WORD	C_DROP			;Drop top value from stack
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DEPTH:
	define_word(`depth')
C_DEPTH:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_S0			;Push S0 (initial data stack pointer)
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_SPFETCH		;Stack pointer onto stack
	.WORD	C_MINUS
	.WORD	C_2
	.WORD	C_DIV
	.WORD	C_1MINUS		;1 minus
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DLESSTHAN:
	define_word(`d<')
C_DLESSTHAN:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ROT			;3rd valu down to top of stack
	.WORD	C_2DUP			;Dup top 2 values on stack
	.WORD	C_EQUALS
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0040-$			;000Ah
	.WORD	C_2DROP			;Drop top two values from stack
	.WORD	C_ULESS			;IF stack-1 < stack_top leave true flag
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	B0041-$			;0008h
B0040:
	.WORD	C_2SWAP
	.WORD	C_2DROP			;Drop top two values from stack
	.WORD	C_GREATER
B0041:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_0GREATER:
	define_word(`0>')
C_0GREATER:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_ZERO			;Put zero on stack
	.WORD	C_GREATER
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_DOTS:
	define_word(`.s')
C_DOTS:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_CR			;Output [CR][LF]
	.WORD	C_DEPTH
	.WORD	C_0BRANCH		;Add offset to BC if stack top = 0
	.WORD	B0042-$			;0020h
	.WORD	C_SPFETCH		;Stack pointer onto stack
	.WORD	C_2MINUS		;2 minus
	.WORD	C_S0			;Push S0 (initial data stack pointer)
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_2MINUS		;2 minus
	.WORD	C_LDO			;Put start & end loop values on RPP
B0043:
	.WORD	C_I			;Copy LOOP index to data stack
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_DOT
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	0FFFEh
	.WORD	C_PLOOP			;Loop + stack & branch if not done
	.WORD	B0043-$			;FFF4h
	.WORD	C_BRANCH		;Add following offset to BC
	.WORD	S_END5-$		;0011h
B0042:
	.WORD	C_CQUOTE		;Output following string
	.BYTE	S_END5-S_START5
S_START5:
	.ascii	"STACK EMPTY "
S_END5:
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_CODE:
	define_word(`code')
C_CODE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_QEXEC			;Error not if not in execute mode
	.WORD	C_XXX1
	.WORD	C_SPSTORE		;Set initial stack pointer value
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_ENDCODE:
	define_word(`end-code')
C_ENDCODE:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_CURRENT
	.WORD	C_FETCH			;Get word from addr on stack
	.WORD	C_CONTEXT
	.WORD	C_STORE			;Store word at addr
	.WORD	C_QEXEC			;Error not if not in execute mode
	.WORD	C_WHATSTACK		;Check stack pointer, error if not ok
	.WORD	C_SMUDGE
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_NEXT:
	define_immediate_word(`next')
C_NEXT:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	00C3h			;Jump instruction
	.WORD	C_CCOMMA		;Save as 8 bit value
	.WORD	C_LIT			;Puts next 2 bytes on the stack
	.WORD	NEXT			;The address of NEXT
	.WORD	C_COMMA			;Reserve 2 bytes and save n
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_LLOAD:
	define_word(`lload')
C_LLOAD:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_BLOCK			;Get block address
	.WORD	C_LIT			;Enter loop with null
	.WORD	0000h			;
LL_BEGIN:
	.WORD	C_DUP			;Dup key
	.WORD	C_0BRANCH		;If null then don't store
	.WORD	LL_NULL-$		;
	.WORD	C_DUP			;Dup key again
	.WORD	C_LIT			;Compare to [CR]
	.WORD	000Dh			;
	.WORD	C_EQUALS		;
	.WORD	C_0BRANCH		;If not [CR] then jump
	.WORD	LL_STORE-$		;
	.WORD	C_DROP			;Drop the [CR]
	.WORD	C_CL			;Get characters per line
	.WORD	C_PLUS			;Add to current addr
	.WORD	C_CL			;Make CL MOD value
	.WORD	C_NEGATE		;Form 2s complement of n
	.WORD	C_AND			;Mask out bits
	.WORD	C_BRANCH		;Done this bit so jump
	.WORD	NO_STORE-$
LL_STORE:
	.WORD	C_OVER			;Get address to store at
	.WORD	C_STORE			;Save chr
NO_STORE:
	.WORD	C_1PLUS			;Next addres
	.WORD	C_BRANCH		;Done so jump
	.WORD	LL_CHAR-$		;
LL_NULL:
	.WORD	C_DROP			;Was null so drop it
LL_CHAR:
	.WORD	C_KEY			;Get key
	.WORD	C_DUP			;Duplicate it
	.WORD	C_LIT			;Compare with [CTRL] Z
	.WORD	001Ah			;
	.WORD	C_EQUALS		;
	.WORD	C_0BRANCH		;If not EOF then jump
	.WORD	LL_BEGIN-$		;
	.WORD	C_DROP			;Drop EOF character
	.WORD	C_DROP			;Drop next address
	.WORD	C_STOP			;Pop BC from return stack (=next)

W_RESET:
	define_word(`reset')
	.WORD	2+$			;Vector to code
aSystemReset:
	LD   C,0x00         ;API 0x00
	RST  0x30           ;  = System reset
	JP   C_STOP

W_TASK:
	define_word(`task') ; empty word -- is this customary marker in the dictionary?
C_TASK:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_STOP			;Pop BC from return stack (=next)
W_TASKEND:


W_EDITI: ; what is that? looks like a start pointer for editor dictionary
; currently it contains only one "clear" word... WIP?

W_CLEAR:				;Clear block n
	define_word(`clear')
C_CLEAR:
	.WORD	E_COLON			;Interpret following word sequence
	.WORD	C_DUP			;Duplicate number
	.WORD	C_SCR			;Get SCR addr
	.WORD	C_STORE			;Store screen number
	.WORD	C_BLOCK			;Get the address of the block
	.WORD	C_BBUF			;Put number of bytes/block on stack
	.WORD	C_ERASE			;Clear the block
	.WORD	C_STOP			;Pop BC from return stack (=next)

CF_UKEY:				;Get key onto stack
	.WORD	2+$			;Vector to code
	CALL	CHR_RD			;User key in routine
	LD	L,A			;Put key on stack
	LD	H,00h			;
	JP	NEXTS1			;Save & NEXT

CF_UEMIT:				;Chr from stack to output
	.WORD	2+$			;Vector to code
	POP	HL			;Get CHR to output
	LD	A,L			;Put in A
	PUSH	BC			;Save regs
	PUSH	DE			;
	CALL	CHR_WR			;User output routine
	POP	DE			;Restore regs
	POP	BC			;
	JP	NEXT			;

CF_UCR:					;CR output
	.WORD	2+$			;Vector to code
	PUSH	BC			;Save regs
	PUSH	DE			;Just in case
	LD	A,0Dh			;Carrage return
	CALL	CHR_WR			;User output routine
	LD	A,0Ah			;Line feed
	CALL	CHR_WR			;User output routine
	POP	DE			;Get regs back
	POP	BC			;
	JP	NEXT			;Next

CF_UQTERMINAL:				;Test for user break
	.WORD	2+$			;Vector to code
	PUSH	BC			;Save regs
	PUSH	DE			;Just in case
	CALL	BREAKKEY		;User break test routine
	POP	DE			;Get regs back
	POP	BC			;
	LD	H,00h			;Clear H
	LD	L,A			;Result in L
	JP	NEXTS1			;Store it & Next

;==============================================================================
; Serial I/O routines
; Rachel - To use INT32K.ASM
;==============================================================================

CHR_RD:					;Character in
	RST 10h
	RET

BREAKKEY:
 	XOR	A			;Wasn't break, or no key, so clear
	RET

CHR_WR:					;Character out
    AND 7Fh         ;To knock off end of word marker
	; if I want to support UTF-8 this will need to be solved better
    RST 08h
	RET

.section .bss
;==============================================================================
;		.ORG	0FE00h	        ;Set up system variable addresses
;==============================================================================
SYSTEM:					;Start of scratch pad area
		.space	6		;User bytes -- uninitialized
; following 16 bytes are initialized by C_WARM
S0:		.space	2		;Initial value of the data stack pointer (DATA_STACK)
R0:		.space	2		;Initial value of the return stack pointer (SYSTEM)
TIB:		.space	2		;Address of the terminal input buffer (DATA_STACK)
WIDTH:		.space	2		;Number of letters saved in names (31)
WARNING:	.space	2		;Error message control number (0)
FENCE:		.space	2		;Dictionary FORGET protection point (VOCAB_BASE)
DP:		.space	2		;The dictionary pointer (VOCAB_BASE+0Bh)
VOC_LINK:	.space	2		;Most recently created vocabulary (E_FORTH)
; these are not initialized (?)
BLK:		.space	2		;Current block number under interpretation
TOIN:		.space	2		;Offset in the current input text buffer
OUT:		.space	2		;Offset in the current output text buffer
SCR:		.space	2		;Screen number last referenced by LIST
OFFSET:		.space	2		;Block offset for disk drives
CONTEXT:	.space	2		;Pointer to the vocabulary within which
					;dictionary search will first begin
CURRENT:	.space	2		;Pointer to the vocabulary within which
					;new definitions are to be created
STATE:		.space	2		;Contains state of compillation
BASE:		.space	2		;Current I/O base address
DPL:		.space	2		;Number of digits to the right of the
					;decimal point on double integer input
FLD:		.space	2		;Field width for formatted number output
CSP:		.space	2		;Check stack pointer
RHASH:		.space	2		;Location of editor cursor in a text block
HLD:		.space	2		;Address of current output
; START_TABLE is copied here
/*
	.BYTE	81h,0A0h
	.WORD	VOCAB_BASE
	.BYTE	00h,00h			;FLAST
*/
FLAST:		.space	6		;FORTH vocabulary data initialised to FORTH
					;vocabulary
/*
	.BYTE	81h,0A0h
	.WORD	W_EDITI
	.WORD	E_FORTH			;ELAST
*/
ELAST:		.space	6		;Editor vocabulary data initialised to
					;EDITOR vocabulary
;	.BYTE	00h			;CRFLAG
CRFLAG:		.space	1		;Carriage return flag
;	.BYTE	00h			;Free
			.space	1		;User byte
/*
	IN	A,(00h)			;I/O Port input
	RET				;routine
*/
PAT:		.space	3		;I/O port fetch routine (input)
/*
	OUT	(00h),A			;I/O Port output
	RET				;routine
*/
PST:		.space	3		;I/O port store routine (output)
;	.WORD	SYSTEM 			;Return stack pointer
RPP:		.space	2		;Return stack pointer
;	.WORD	MASS_STORE		;Mass storage buffer to use
USE:		.space	2		;Mass storage buffer address to use
;	.WORD	MASS_STORE		;Storage buffer just used
PREV:		.space	2		;Mass storage buffer address just used
;	.BYTE	00h				;Interrupt flag
INTFLAG:	.space	1		;Interrupt flag
;	.BYTE	00h			;Free
			.space	1		;User byte
;	.WORD	C_ABORT			;Interrupt vector
INTVECT:	.space	2		;Interrupt vector
;	.WORD	CF_UQTERMINAL		;C field address ?TERMINAL
UTERMINAL:	.space	2		;Code field address of word ?TERMINAL
;	.WORD	CF_UKEY			;C field address KEY
UKEY:		.space	2		;Code field address of word KEY
;	.WORD	CF_UEMIT		;C field address EMIT
UEMIT:		.space	2		;Code field address of word EMIT
;	.WORD	CF_UCR			;C field address CR
UCR:		.space	2		;Code field address of word CR
;	.WORD	CF_URW			;C field address R/W
URW:		.space	2		;Code field address of word R/W
;	.WORD	CF_UABORT		;C field address ABORT
UABORT:		.space	2		;Code field address of word ABORT
;	.WORD	0020h			;CHRs per input line
UCL:		.space	2		;Number of characters per input line
;	.WORD	DISK_START		;Pseudo disk buf start
UFIRST:		.space	2		;Start of pseudo disk buffer
;	.WORD	DISK_END		;Pseudo disk buf end
ULIMIT:		.space	2		;End of pseudo disk buffer
;	.WORD	BLOCK_SIZE		;Bytes per block
UBBUF:		.space	2		;Number of bytes per block
;	.WORD	BUFFERS			;Buffers per block
UBSCR:		.space	2		;Number of buffers per block
; not used...
KEYBUF:		.space	2		;Double key buffer
RAF:		.space	2		;Register AF
RBC:		.space	2		;Register BC
RDE:		.space	2		;Register DE
RHL:		.space	2		;Register HL
RIX:		.space	2		;Register IX
RIY:		.space	2		;Register IY
RAF2:		.space	2		;Register AF'
RBC2:		.space	2		;Register BC'
RDE2:		.space	2		;Register DE'
RHL2:		.space	2		;Register HL'
		.space	1		;User byte
JPCODE:		.space	1		;JMP code (C3) for word CALL
JPVECT:		.space	2		;JMP vector for word CALL
		.space	32		;User bytes

;==============================================================================
