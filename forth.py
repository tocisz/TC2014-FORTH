import re

chunks = []
label_of = dict()

def esc(s):
	return s.replace("\\", r'\134').replace('"', r'\042').replace("\0", r'\000')

def head(label, name, immediate):
	print(f"""W_{label}:
	.set this_word, .
	.byte {'0C0' if immediate else '80'}h+{len(name)}
	.ascii "{esc(name)}"
	.word last_word_address
	.set last_word_address, this_word
C_{label}:""")

def print_asm_word(label, name, code, immediate):
	head(label, name,  immediate)
	print(f"	.WORD	2+$", end='')
	print(code)

def find(prefix, word):
	if word in label_of:
		return f"{prefix}_{label_of[word]}"
	else:
		return word

class CurlySubst:
	def __init__(self):
		self.i = -1
		self.replacements = []
		self.pattern = r"\{(.*?)\}"
	def eval_curly(self, m):
		self.i = self.i + 1
		t = m.group(1)
		if (t[0],t[-1]) in [("'","'"),('"','"')]:
			t = self.string(t[1:-1])
		self.replacements.append(t)
		return f"{{{self.i}}}"
	def string(self,t):
		return f"\t.BYTE\t{len(t)}\n\t.ASCII\t\"{esc(t)}\""
def print_word(label, name, words, immediate, no_head):
	curly = CurlySubst()
	if no_head:
		print(f"{label}:")
	else:
		head(label, name, immediate)
	words = re.sub(curly.pattern, curly.eval_curly, words.strip())
	ws = re.split(r'\s+', words.strip())
	print(f"	.WORD	{find('X', ws[0])}")
	for w in ws[1:]:
		m = re.match(curly.pattern, w) 
		if m:
			print(curly.replacements[int(m.group(1))])
		else:
			print(f"	.WORD	{find('C', w)}")
	print()

def print_def_word(label, name, words, code, immediate):
	head(label, name, immediate)
	ws = re.split(r'\s+', words)
	print(f"	.WORD	{find('X', ws[0])}")
	for w in ws[1:]:
		print(f"	.WORD	{find('C', w)}")
	print(code)

def verbatim(text):
	chunks.append([print, text])

def word(l_n, words, immediate=False, no_head=False):
	[label, name] = l_n.split(':',1)
	label_of[name] = label
	chunks.append([print_word, label, name, words, immediate, no_head])

def asm_word(l_n, code, immediate=False):
	[label, name] = l_n.split(':',1)
	label_of[name] = label
	chunks.append([print_asm_word, label, name, code, immediate])

def def_word(l_n, words, code, immediate=False):
	[label, name] = l_n.split(':',1)
	label_of[name] = label
	chunks.append([print_def_word, label, name, words, code, immediate])

verbatim("""; TC2014-FORTH
; 
; This Forth system is based on an old [fig-Forth derivative](https://github.com/rachel8973/RC2014-FORTH)
; for Zilog Z80.
; 
; I did some modifications I liked:
; 1. Don't mark end of word name with `+0x80`. You can use Unicode in word names if you like.
;    It's (still) case sensitive unlike most of Forth systems.
; 2. All words are lowercase. I find it more aesthetically pleasing.
; 3. Compiled by a standard GNU Assembler.
; 4. Python is used as a macro generator. It can generate assembly from word definitions
;    like `word("FIRST:first", ": ufirst @ ;s")`, which makes reading and modyfiying source code easier.
; 5. With Python used for generating word definitions it's possible to generate graph of dependencies between words.
; 6. Cleanup of hacks for defining vocabularies that were used in the Forth system on which it's based.
;    Now `forth` is the last word and its copied to RAM, so `current` address can change witin it.
; 7. More standard `words` instead of `VLIST`. Added `vocs`.
; 8. Restore fig-Forth `create` and `<builds`.
; 9. Simpler `sp!` and `rp!`.
; 10. Linux terminal CLS.
; 11. Using IX for return stack pointer.
; 12. `sysdump` and `sysload` to dump and load system state.
; 13. Added `rdrop`, `cmove>` and `recurse`.
; 14. 16-bit unsigned bit shifts: `2u*`, `2u/` and `cell`, `cell+`, `cells`.

; Build options
.ifnotdef INTERRUPTS
INTERRUPTS	= 0;
.endif
.ifnotdef BLOCKS
BLOCKS		= 0;
.endif
.ifnotdef NATIVECALL
NATIVECALL	= 0;
.endif
SYSDUMP_LOAD	= 1;

.section .cold

	JP	X_COLD

.section .warm

	JP	X_COLD

INIT_TABLE: ; copied by warm/cold to S0 and onward
	.WORD	DATA_STACK		;data stack - grows down
DEF_SYSADDR:
	.WORD	SYSTEM			;return stack - grows down
	.WORD	DATA_STACK		;TIB - grows up
	.WORD	001Fh			;Word name length (default 31)
	.WORD	0000h			;Error message control number
	.WORD	VOCAB_BASE		;FORGET protection
; following copied only by COLD
	.WORD	VOCAB_BASE+(W_FORTH_END-W_FORTH) ;Dictionary pointer -- this must be >= length of a last word
	.WORD	VOCAB_BASE+(E_FORTH-W_FORTH)	;Most recently created vocab, in RAM

START_TABLE:
	.BYTE	00h			;CRFLAG
	.BYTE	00h			;Free
	IN	A,(00h)			;I/O Port input
	RET				;routine
	OUT	(00h),A			;I/O Port output
	RET				;routine
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
START_TABLE_END:

; assuming:
;      BC -> i
;      IX -> r
;      DE -> p
;
; p = *i + 1/2 -- half way between CFA and PFA
; goto(**(i++))
NEXTS2:
	PUSH	DE
NEXTS1:
	PUSH	HL
NEXT:
.if INTERRUPTS
	LD	A,(INTFLAG)		;Interrupt flag
	BIT	7,A			;Check for interrupt
	JR	Z,NOINT			;No interrupt
	BIT	6,A			;Interrupt enabled ?
	JR	NZ,NOINT		;No interrupt
	LD	HL,(INTVECT)		;Get interrupt vector
	LD	A,40h			;Clear flag byte
	LD	(INTFLAG),A		;Interrupt flag into HL
	JR	NEXTADDR		;JP (HL)
.endif
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
	EX	DE,HL 			; ADJUST_HERE
	JP	(HL) 			;Jump to it

.set last_word_address, 0000h		;First word in vocabulary
""")

asm_word("LIT:lit", """
	LD	A,(BC)			;Gets next word from (BC)
	INC	BC			;then increments BC to point
	LD	L,A			;to the next addr. Pushes the
	LD	A,(BC)			;result onto the stack.
	INC	BC			;
	LD	H,A			;
	JP	NEXTS1			;Save & NEXT
""")

asm_word("EXECUTE:execute", """
	POP	HL			;Get addr off data stack
	JP	NEXTADDR		;Basically JP (HL)
""")

asm_word("BRANCH:branch", """
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
""")

asm_word("0BRANCH:0branch", """
	POP	HL			;Get value off stack
	LD	A,L			;Set flags
	OR	H			;
	JR	Z,X_BRANCH		;If zero then do the branch
	INC	BC			;Else dump branch address
	INC	BC			;
	JP	NEXT			;Continue execution
""")

asm_word("LLOOP:(loop)", """
	LD	DE,0001
C_ILOOP:
	LD	A,D			; A incerement high
	EX	DE,HL
	LD	E,(IX)
	LD	D,(IX+1)
	ADD	HL,DE
	EX	DE,HL			; DE = incremented loop counter
	LD	(IX),E
	LD	(IX+1),D		; Update it
	EX	DE,HL			; HL = incremented loop counter
	LD	E,(IX+2)
	LD	D,(IX+3)		; limit in DE
	OR	A			; Is increment negative? Clear carry BTW
	JP	P,INCR_LOOP
	EX	DE,HL			;Reverse comparison for negative increment
INCR_LOOP:
	SBC	HL,DE		; i-limit (step positive) [or limit-i (step negative)]
				; so it's negative before limit is reached
	JP	M,X_BRANCH		;Not reached limit so jump
	INC	IX			;Drop index & limit from return stack
	INC	IX
	INC	IX
	INC	IX
	INC	BC			;Skip branch offset
	INC	BC			;
	JP	NEXT
""")

asm_word("PLOOP:(+loop)", """
	POP	DE			;Get value from stack
	JR	C_ILOOP			;Go do loop increment
""")

asm_word("LDO:(do)", """
	DEC	IX			;Add space for two values
	DEC	IX			;
	DEC	IX			;
	DEC	IX			;
	POP	DE
	LD	(IX),E
	LD	(IX+1),D
	POP	DE
	LD	(IX+2),E
	LD	(IX+3),D
	JP	NEXT
""")

asm_word("I:i", """
X_I:
	LD	L,(IX)			;Get LOOP index off return stack
	LD	H,(IX+1)		;
	JP	NEXTS1
""")

asm_word("J:j", """
	LD	L,(IX+4)		;Get LOOP index off return stack
	LD	H,(IX+5)		;
	JP	NEXTS1
""")

asm_word("DOUBLERFETCH:2r@", """
	LD	E,(IX)
	LD	D,(IX+1)
	LD	L,(IX+2)
	LD	H,(IX+3)
	JP	NEXTS2
""")

# Convert the character c according to base n1 to a binary number n2
# with a true flag on top of stack. If the digit is an invalid character,
# only a false flag is left on stack.
# ( c n1 -- n2 tf or ff )
asm_word("DIGIT:digit", """
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
""")

# Search the dictionary starting at the address on stack for a name
# matching the text at the address second on stack. Return the parameter field address
# of the matching name, its length byte, and a boolean true flag on stack for a match.
# If no match is possible, only a boolean false flag is left on stack.
asm_word("FIND:(find)", """
	POP	DE			;Get pointer to next vocabulary word
COMPARE:
	POP	HL			;Copy pointer to word we're looking 4
	push	bc		; 1. BC backup (must be restored before NEXT)
	LD	A,(DE)			;Get vocabulary word length+flags
	AND	3Fh			;Ignore start and immediate flag
	ld	b,0
	ld	c,a		; BC is length... not exactly, for smudged it's length+32
	res	5,c		; clear smudge
	PUSH	HL		; 2. word to find
	push	de		; 3. vocabulary word NFA
	ex	de,hl
	add	hl,bc
	ld	b,h
	ld	c,l		; BC is LFA-1
	pop	hl
	push	hl	; HL - NFA, DE - to find
	ex	de,hl	; HL - to find, DE - NFA
	XOR	(HL)			;Compare with what we've got
	JR	NZ,NO_MATCH		;No match so skip to next word
MATCH_NO_END:
	; s: BC, dictionary word, word to find; BC: LFA-1, DE: dict word ptr, HL: word ptr
	; if BC = DE then it's a MATCH
	ld	a,c
	xor	e
	jr	z,IS_END2 ;  usually it's not the end
CONTINUE:
	INC	DE			;Compare next chr
	INC	HL			;Compare next chr
	LD	A,(DE)			;
	AND	7Fh			;Ignore freaking flag (for now)
	XOR	(HL)			;
	jr	NZ,NO_MATCH		;No match jump
	JR	MATCH_NO_END		;Match & not last, so next chr
IS_END2:
	ld	a,b
	xor	d
	jr	nz,CONTINUE ; if first byte is 0 usually second too
MATCH:
	pop	hl		; 3. NFA
	pop	de		; 2. word to find - discard it
	ld	d,0
	ld	e,(hl)		; return(2) word header
	ld	hl,5
	add	hl,bc
	pop	bc		; 1. BC - OK
	push	hl		; return(3) PFA
	LD	HL,1		; return(1) TRUE
	JP	NEXTS2			;Save both & NEXT
NO_MATCH:
	; s: BC, dictionary word, word to find; BC: LFA-1
	pop	hl		; 3. NFA - discard it
	pop	de		; 2. word to find
	inc	bc
	ld	h,b
	ld	l,c		; HL - LFA
	pop	bc		; 1. BC - OK
	push	de		; word to find -> needed by COMPARE
	; s: word to find, HL: LFA
	LD	E,(HL)			;Vector into DE
	INC	HL			;
	LD	D,(HL)			;
	LD	A,D			;Check it's not last (first) word
	OR	E			;
	; s: word to find, DE: next word NFA
	JR	NZ,COMPARE		;No error so loop
	POP	HL			;Dump pointer
	LD	HL,0000			;Flag error
	JP	NEXTS1			;Save & NEXT
""")

# A primitive word to scan the text. From the byte address and the delimiter c,
# it determines the byte offset to the first non-delimiter character,
# the offset to the first delimiter after the text string,
# and the offset to the next character after the delimiter.
# If the string is delimited by a NUL, the last offset is equal to the previous offset.
# ( addr c --- addr n1 n2 n3 )
asm_word("ENCLOSE:enclose", """
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
""")

word("EMIT:emit", ": uemit @ execute 1 out +! ;s")

asm_word("KEY:key", """
	LD	HL,(UKEY)		;Get the vector
	JP	NEXTADDR		;Execute
""")

asm_word("QKEY:?key", """
	LD	HL,(serRxBufUsed)	;alternatively, could call RST 18
	JP	NEXTS1
""")

word("TERMINAL:?terminal", ": u?terminal @ execute ;s")
word("CR:cr", ": ucr @ execute ;s")
word("CLS:cls", ": lit ESC emit lit 'c' emit ;s") 

# ( src dst n -- )
asm_word("CMOVE:cmove", """
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
""")

# ( src dst n -- )
# same as cmove, but start from the right
asm_word("CMOVER:cmove>", """
	LD	L,C			;Save BC for now
	LD	H,B			;
	POP	BC			;Get no. of bytes to move
	POP	DE			;Get destination address
	EX	(SP),HL			;Get source address, push BC btw
	LD	A,B			;Check it's not a 0 length block
	OR	C			;
	JR	Z,NO_BYTESR		;If 0 length then do nothing
	ADD	HL,BC
	DEC	HL			;Increment HL to end of range
	EX	DE,HL
	ADD	HL,BC
	DEC	HL
	EX	DE,HL			;Increment DE to end of range
	LDDR				;Move block (from right end)
NO_BYTESR:
	POP	BC			;Get BC back
	JP	NEXT
""")

asm_word("USTAR:u*", """
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
""")

asm_word("UMOD:u/mod", """
	LD	HL,0004
	ADD	HL,SP	;this looks dangerous, but it looks to be above SP
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
""")

asm_word("AND:and", """
	POP	DE			;Get n1 off stack
	POP	HL			;Get n2 off stack
	LD	A,E			;AND lo bytes
	AND	L			;
	LD	L,A			;Result in L
	LD	A,D			;AND hi bytes
	AND	H			;
	LD	H,A			;Result in H
	JP	NEXTS1			;Save & next
""")

asm_word("OR:or", """
	POP	DE			;Get n1 off stack
	POP	HL			;Get n2 off stack
	LD	A,E			;OR lo bytes
	OR	L			;
	LD	L,A			;Result in L
	LD	A,D			;OR hi bytes
	OR	H			;
	LD	H,A			;Result in H
	JP	NEXTS1			;Save & next
""")

asm_word("XOR:xor", """
	POP	DE			;Get n1 off stack
	POP	HL			;Get n2 off stack
	LD	A,E			;XOR lo bytes
	XOR	L			;
	LD	L,A			;Result in L
	LD	A,D			;XOR hi bytes
	XOR	H			;
	LD	H,A			;Result in H
	JP	NEXTS1			;Save & NEXT
""")

asm_word("SPFETCH:sp@", """
	LD	HL,0000			;No offset
	ADD	HL,SP			;Add SP to HL
	JP	NEXTS1			;Save & NEXT
""")

asm_word("SPSTORE:sp!", """
	LD	SP,(S0)			;Set SP
	JP	NEXT
""")

asm_word("RPFETCH:rp@", """
	LD	D,IXH
	LD	E,IXL
	EX	DE,HL
	JP	NEXTS1			;Save & NEXT
""")

asm_word("RPSTORE:rp!", """
	LD	IX,(R0)
	JP	NEXT
""")

# ; assuming:
# ;      BC -> i
# ;   (RPP) -> r
# ;      DE -> p
# ;
# ; i = *(r++) -- pop i from the return stack
# ; goto(NEXT)
asm_word("STOP:;s", """
X_STOP:
	LD	C,(IX)			;Get low byte
	LD	B,(IX+1)		;Get high byte
	INC	IX			;
	INC	IX			;
	JP	NEXT
""")

# this "leave" is non-standard
# it's not breaking the loop immediately
asm_word("LEAVE:leave", """
	LD	A,(IX)
	LD	(IX+2),A
	LD	A,(IX+1)
	LD	(IX+3),A
	JP	NEXT
""")

asm_word("MOVER:>r", """
	POP	DE			;Get value
	DEC	IX
	DEC	IX
	LD	(IX),E
	LD	(IX+1),D
	JP	NEXT
""")

asm_word("RMOVE:r>", """
	LD	E,(IX)
	LD	D,(IX+1)
	INC	IX
	INC	IX
	PUSH	DE			;Push on data stack
	JP	NEXT
""")

asm_word("RDROP:rdrop", """
	INC	IX
	INC	IX
	JP	NEXT
""")

word("RFETCH:r@", "i")

asm_word("0EQUALS:0=", """
X_0EQUALS:
	POP	HL			;Get value from stack
	LD	A,L			;set flags
	OR	H			;
	LD	HL,0000			;Not = 0 flag
	JR	NZ,NO_ZERO		;
	INC	HL			;= 0 flag
NO_ZERO:
	JP	NEXTS1			;Save & NEXT
""")

word("NOT:not", "0=")

asm_word("0LESS:0<", """
	POP	HL			;Get value
	ADD	HL,HL			;S bit into C
	LD	HL,0000			;Wasn't < 0 flag
	JR	NC,NOT_LT0		;
	INC	HL			;Was < 0 flag
NOT_LT0:				;
	JP	NEXTS1			;Save & NEXT
""")

asm_word("PLUS:+", """
	POP	DE			;Get n2
	POP	HL			;Get n1
	ADD	HL,DE			;Add them
	JP	NEXTS1			;Save & NEXT
""")

asm_word("DPLUS:d+", """
	LD	HL,0006			; offset to low word	
	ADD	HL,SP			; add stack pointer
	; Looks OK, but not sure
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
""")

asm_word("NEGATE:negate", """
	POP	HL			;Get number
	LD	A,L			;Low byte into A
	CPL				;Complement it
	LD	L,A			;Back into L
	LD	A,H			;High byte into A
	CPL				;Complement it
	LD	H,A			;Back into H
	INC	HL			;+1
	JP	NEXTS1			;Save & NEXT
""")

asm_word("DNEGATE:dnegate", """
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
""")

asm_word("OVER:over", """
	POP	DE			;Get top
	POP	HL			;Get next
	PUSH	HL			;Save it back
	JP	NEXTS2			;Save both & NEXT
""")

asm_word("DROP:drop", """
	POP	HL			;Get top value
	JP	NEXT
""")

asm_word("2DROP:2drop", """
	POP	HL			;Get top value
	POP	HL			;Get top value
	JP	NEXT
""")

asm_word("SWAP:swap",  """
	POP	HL			;Get top value
	EX	(SP),HL			;Exchanhe with next down
	JP	NEXTS1			;Save & NEXT
""")

asm_word("BSWAP:bswap",  """
	POP	HL			;Get top value
	LD	A,L
	LD	L,H
	LD	H,A
	JP	NEXTS1			;Save & NEXT
""")

asm_word("DUP:dup", """
	POP	HL			;Get value off stack
	PUSH	HL			;Copy it back
	JP	NEXTS1			;Save & NEXT
""")

asm_word("2DUP:2dup", """
	POP	HL			;Get top two values from stack
	POP	DE			;
	PUSH	DE			;Copy them back
	PUSH	HL			;
	JP	NEXTS2			;Save both & NEXT
""")

# ( addr n - addr+n addr )
asm_word("BOUNDS:bounds", """
	POP	HL			; get n
	POP	DE			; get addr
	ADD	HL,DE			; add addr to n
	EX	DE,HL			; swap them
	JP	NEXTS2			; save both & NEXT
""")

asm_word("PLUSSTORE:+!", """
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
""")

asm_word("TOGGLE:toggle", """
	POP	DE			;Get byte
	POP	HL			;Get addr
	LD	A,(HL)			;Get byte from addr
	XOR	E			;Toggle it
	LD	(HL),A			;Save result
	JP	NEXT
""")

asm_word("FETCH:@", """
	POP	HL			;Get addr
	LD	E,(HL)			;Get low byte
	INC	HL			;
	LD	D,(HL)			;Get high byte
	PUSH	DE			;Save it
	JP	NEXT
""")

asm_word("CFETCH:c@", """
	POP	HL			;Get addr
	LD	L,(HL)			;Get byte
	LD	H,00h			;Top byte = 0
	JP	NEXTS1			;Save & NEXT
""")

asm_word("2FETCH:2@", """
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
""")

asm_word("STORE:!", """
	POP	HL			;Get addr
	POP	DE			;Get word
	LD	(HL),E			;Store low byte
	INC	HL			;
	LD	(HL),D			;Store high byte
	JP	NEXT
""")

asm_word("CSTORE:c!", """
	POP	HL			;Get addr
	POP	DE			;Get byte
	LD	(HL),E			;Save it
	JP	NEXT
""")

asm_word("2STORE:2!", """
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
""")

# ; this is the most important part of Forth
# ; this function starts interpretation of a standard Forth threaded word
# ; (RPP) - top of the return stack (grows down)
# ; SP - top of the stack (grows up)
# ; BC - word pointer
# ;
# ; assuming:
# ;      BC -> i
# ;   (RPP) -> r
# ;      DE -> p
# ;
# ; *(--r) = i -- push i to the return stack
# ; i = p + 1/2 -- PFA(0)
# ; goto(NEXT)
def_word("COLON::", ": ?exec !csp current @ context ! create ] (;code)", """
X_COLON:
	DEC	IX
	DEC	IX
	LD	(IX),C
	LD	(IX+1),B
	INC	DE
	LD	C,E
	LD	B,D
	JP	NEXT
""")

word("SEMICOLON:;", ": ?comp ?csp compile ;s smudge [ ;s", immediate=True)

def_word("CONSTANT:constant", ": create smudge , (;code)", """
X_CONSTANT:				;Put next word on stack
	INC	DE			;Adjust pointer
	EX	DE,HL			;Get next word
	LD	E,(HL)			;
	INC	HL			;
	LD	D,(HL)			;
	PUSH	DE			;Put on stack
	JP	NEXT
""")

def_word("VARIABLE:variable", ": 0 constant (;code)", """
X_VARIABLE:
	INC	DE		; Interesting that every entrypoint need to adjust it
				; Couldn't it be adjusted there ADJUST_HERE ?
				; Well, most of asm coded words just ignore it... 
	PUSH	DE
	JP	NEXT
""")

def_word("USER:user", ": constant (;code)", """
X_USER:
	INC	DE			;Adjust to next word
	EX	DE,HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	LD	HL,(DEF_SYSADDR)
	ADD	HL,DE
	JP	NEXTS1			;Save & NEXT
""")

word("ZERO:0", "constant 0000h")
word("1:1", "constant 0001h")
word("2:2", "constant 0002h")
word("3:3", "constant 0003h")
word("BL:bl", "constant 0020h")
word("CL:c/l", ": uc/l @ ;s")
word("FIRST:first", ": ufirst @ ;s")
word("LIMIT:limit", ": ulimit @ ;s")

verbatim("""
.if BLOCKS
""")
word("BBUF:b/buf", ": ub/buf @ ;s")
word("BSCR:b/scr", ": ub/scr @ ;s")
verbatim("""
.endif
""")

word("S0:s0", "user S0-SYSTEM")
word("R0:r0", "user R0-SYSTEM")
word("TIB:tib", "user TIB-SYSTEM")
word("WIDTH:width", "user WIDTH-SYSTEM")
word("WARNING:warning", "user WARNING-SYSTEM")
word("FENCE:fence", "user FENCE-SYSTEM")
word("DP:dp", "user DP-SYSTEM")
word("VOC_LINK:voc-link", "user VOC_LINK-SYSTEM")

verbatim("""
.if BLOCKS
""")
word("BLK:blk", "user BLK-SYSTEM")
verbatim("""
.endif
""")

word("TOIN:>in", "user TOIN-SYSTEM") # current position in TIB
word("OUT:out", "user OUT-SYSTEM")
word("SCR:scr", "user SCR-SYSTEM")
word("OFFSET:offset", "user OFFSET-SYSTEM")
word("CONTEXT:context", "user CONTEXT-SYSTEM")
word("CURRENT:current", "user CURRENT-SYSTEM")
word("STATE:state", "user STATE-SYSTEM")
word("BASE:base", "user BASE-SYSTEM")
word("DPL:dpl", "user DPL-SYSTEM")
word("FLD:fld", "user FLD-SYSTEM")
word("CSP:csp", "user CSP-SYSTEM")
word("RHASH:r#", "user RHASH-SYSTEM")
word("HLD:hld", "user HLD-SYSTEM")
word("UCL:uc/l", "user UCL-SYSTEM")
word("UFIRST:ufirst", "user UFIRST-SYSTEM")
word("ULIMIT:ulimit", "user ULIMIT-SYSTEM")
word("UBBUF:ub/buf", "user UBBUF-SYSTEM")
word("UBSCR:ub/scr", "user UBSCR-SYSTEM")
word("UTERMINAL:u?terminal", "user UTERMINAL-SYSTEM")
word("UKEY:ukey", "user UKEY-SYSTEM")
word("UEMIT:uemit", "user UEMIT-SYSTEM")
word("UCR:ucr", "user UCR-SYSTEM")
word("URW:ur/w", "user URW-SYSTEM")
word("UABORT:uabort", "user UABORT-SYSTEM")

verbatim("""
.if NATIVECALL
""")
word("RAF:raf", "user RAF-SYSTEM")
word("RBC:rbc", "user RBC-SYSTEM")
word("RDE:rde", "user RDE-SYSTEM")
word("RHL:rhl", "user RHL-SYSTEM")
word("RIX:rix", "user RIX-SYSTEM")
word("RIY:riy", "user RIY-SYSTEM")
word("RAF2:raf'", "user RAF2-SYSTEM")
word("RBC2:rbc'", "user RBC2-SYSTEM")
word("RDE2:rde'", "user RDE2-SYSTEM")
word("RHL2:rhl'", "user RHL2-SYSTEM")
word("RA:ra", "user RAF+1-SYSTEM")
word("RF:rf", "user RAF-SYSTEM")
word("RB:rb", "user RBC+1-SYSTEM")
word("RC:rc", "user RBC-SYSTEM")
word("RD:rd", "user RDE+1-SYSTEM")
word("RE:re", "user RDE-SYSTEM")
word("RH:rh", "user RHL+1-SYSTEM")
word("RL:rl", "user RHL-SYSTEM")

asm_word("CALL:call", """
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
.endif
""")

asm_word("1PLUS:1+", """
	POP	HL			; get n
	INC	HL			; add 1
	JP	NEXTS1			; save result & NEXT
""")

asm_word("2PLUS:2+", """
X_2PLUS:
	POP	HL			; get n
	INC	HL			; add 1
	INC	HL			; add 2
	JP	NEXTS1			; save result & NEXT
""")

asm_word("1MINUS:1-", """
	POP	HL			; get n
	DEC	HL			; add 1
	JP	NEXTS1			; save result & NEXT
""")

asm_word("2MINUS:2-", """
X_2MINUS:
	POP	HL			; get n
	DEC	HL			; subtract 1
	DEC	HL			; subtract 2
	JP	NEXTS1			; save result & NEXT
""")

asm_word("UTIMES2:2u*", """
X_UTIMES2:
	POP	HL
	ADD	HL,HL
	JP	NEXTS1			;Save & NEXT
""")

asm_word("UDIV2:2u/", """
	POP	HL
	SRL	H
	RR	L
	JP	NEXTS1			;Save & NEXT
""")

word("CELL:cell", "constant 2h")
word("CELLP:cell+", "2+")
word("CELLS:cells", "2u*")

word("HERE:here", ": dp @ ;s")
word("ALLOT:allot", ": dp +! ;s")
word("COMMA:,", ": here ! 2 allot ;s")
word("CCOMMA:c,", ": here c! 1 allot ;s")

asm_word("MINUS:-", """
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
""")

word("EQUALS:=", ": - 0= ;s")

asm_word("LESSTHAN:<", """
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
""")

word("ULESS:u<", """:
2dup xor 0< 0branch 12
	drop 0< 0=
branch 6
	- 0<
;s""")

word("GREATER:>", ": swap < ;s")

asm_word("ROT:rot", """
	POP	DE			;Top value
	POP	HL			;Next one down
	EX	(SP),HL			;Exchange with third
	JP	NEXTS2			;Save both & NEXT
""")

# pick with negative value could be BAD
word("PICK:pick", ": dup + sp@ + @ ;s")

word("SPACE:space", ": bl emit ;s")

word("QUERYDUP:?dup", ": dup 0branch 4 dup ;s")

word("TRAVERSE:traverse", """:
swap
	over +
lit 127 over c@ < 0branch -16
swap drop
;s""")

word("LATEST:latest", ": current @ @ ;s")
word("LFA:lfa", ": lit 4 - ;s")
word("CFA:cfa", "2-")
word("NFA:nfa", ": lit 5 - lit -1 traverse ;s")
word("PFA:pfa", ": dup c@ lit 31 and + lit 5 + ;s")
word("CSPSTORE:!csp", ": sp@ csp ! ;s")

word("QERROR:?error", ": swap 0branch 8 error branch 4 drop ;s")
word("QCOMP:?comp", ": state @ 0= lit 17 ?error ;s")
word("QEXEC:?exec", ": state @ lit 18 ?error ;s")
word("QPAIRS:?pairs", ": - lit 19 ?error ;s")
word("WHATSTACK:?csp", ": sp@ csp @ - lit 20 ?error ;s")

verbatim("""
.if BLOCKS
""")
word("QLOADING:?loading", ": blk @ 0= lit 22 ?error ;s")
verbatim("""
.endif
""")

word("COMPILE:compile", ": ?comp r> dup 2+ >r @ , ;s")
word("LEFTBRKT:[", ": 0 state ! ;s", immediate=True)
word("RIGHTBRKT:]", ": lit 192 state ! ;s")
word("SMUDGE:smudge", ": latest lit 32 toggle ;s")

word("HEX:hex", ": lit 16 base ! ;s")
word("DECIMAL:decimal", ": lit 10 base ! ;s")

word("CCODE:(;code)", ": r> latest pfa cfa ! ;s")
# why no smudge in ;code ? I guess end-code can be used to finalize it
word("SCCODE:;code", ": ?csp compile (;code) [ ;s", immediate=True)
word("BUILDS:<builds", ": 0 constant ;s")
def_word("DOES:does>", ": r> latest pfa ! (;code)", """
X_DOES:
	DEC	IX
	DEC	IX
	LD	(IX),C
	LD	(IX+1),B
	INC	DE
	EX	DE,HL
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	INC	HL
	JP	NEXTS1			;Save & NEXT
""")

word("COUNT:count", ": dup 1+ swap c@ ;s") # Convert string at addr to addr + length

# ( addr len - )
word("TYPE:type", """:
?dup 0branch 24
	over + swap
	(do)
		i c@ emit
	(loop) -8
branch 4
	drop
;s""")

word("TRAILING:-trailing", """:
dup 0
(do)
	over over
	+ 1-
	c@ bl -
	0branch 8
		leave
	branch 4
		1-
(loop) -28
;s""")

word("CQUOTE:(.\")",""":
r@
count
dup 1+
r> + >r
type
;s""")

word("QUOTE:.\"", """:
lit 34
state @
0branch 18
	compile (.")
	word c@ 1+ allot
branch 8
	word count type
;s""", immediate=True)

word("EXPECT:expect", """:
over + over
(do)
	key dup lit BKSP =
	0branch 40
		drop
		dup i =
		dup r>
		2- +
		>r
		0branch 10
			lit BEL
		branch 6
			lit BKSP
	branch 40
		dup lit CR =
		0branch 14
			leave
			drop bl 0
		branch 4
			dup
		i c!
		0 i 1+ !
	emit
(loop) -94
drop
;s""")

word("QUERY:query", """:
tib @
lit 80 expect
0 >in !
;s""")

word("NULL:\0", """:
{.if BLOCKS}
blk @
0branch 40
	1 blk +!
	0 >in !
	blk @
	b/scr 1- and 0=
	0branch 8
		?exec r> drop
branch 6
{.endif}
	r> drop
;s""", immediate=True)

# ( addr n byte -- )
asm_word("FILL:fill", """
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
""")

word("ERASE:erase", ": 0 fill ;s")
word("BLANKS:blanks", ": bl fill ;s")
word("HOLD:hold", ": lit -1 hld +! hld @ c! ;s")
word("PAD:pad", ": here lit 68 + ;s")

# Read one word from input stream, delimitedby c.
# ( c - )
word("WORD:word", """:
{.if BLOCKS}
blk @
0branch 12
	blk @ block
branch 6
{.endif}
tib @ >in @ +
swap enclose
here lit 34 blanks
>in +!
over - >r r@ here c! + here 1+ r>
cmove here
;s""")

# (NUMBER) is the run-time routine of number conversion.
# It converts an ASCII text beginning at addr1+1 according to BASE.
# The result is accumulated with d1 to become d2.
# addr2 is the address of the first unconvertable digit.
# ( d1 addr1 --- d2 addr2 )
word("CONVERT:convert", """:
	1+ dup >r c@ base @ digit
	0branch 44
	swap base @ u* drop rot base @ u* d+
	dpl @ 1+
	0branch 8
		1 dpl +!
	r>
branch -58
r>
;s""")

# NUMBER converts character string at addr with a preceding byte count
# to signed double integer number, using the current base.
# If a decimal point is encountered in the text, its position will be given in DPL.
# If numeric conversion is not possible, issue an error message.
# This implementation ignores byte count and parses until space.
# ( addr – d )
word("NUMBER:number", """:
0 0 rot
dup 1+ c@
lit 45 =
dup >r
+
lit -1
	dpl !
	convert
	dup c@ bl - 0branch 22
	dup c@
	lit 46 - 0 ?error
	0
branch -36
drop
r> 0branch 4
	dnegate
;s""")

# why context is not enough? why also search in latest?
word("MFIND:-find", """:
bl word
context @ @
(find)
dup 0= 0branch 10
	drop here
	latest (find)
;s""")

word("CABORT:(abort)", ": abort ;s")

word("ERROR:error", """:
warning @ 0<
0branch 4
	(abort)
here count type (.") {"? "} message
sp!
{.if BLOCKS}
blk @ ?dup
0branch 8
	>in @ swap
{.endif}
quit""")

word("ID:id.", ": count lit 31 and type space ;s")

# this is "create" from the book, but with "-find" instead of "bl word"
# that's OK, because -find also does "bl word"
# this only informs that word is redefined
word("CREATE:create", """:
-find 0branch 16
	drop
	nfa id.
	lit 4 message
	space
here
dup c@
width @ min
1+ allot
dup lit 160 toggle
latest ,
current @ !
here 2+ ,
;s""")

word("CCOMPILE:[compile]", ": -find 0= 0 ?error drop cfa , ;s")

word("LITERAL:literal", """:
state @ 0branch 8
	compile lit ,
;s""", immediate=True)

word("DLITERAL:dliteral", """:
state @ 0branch 8
	swap literal literal
;s""", immediate=True)

word("QSTACK:?stack", """:
sp@
s0 @ swap
u< 1 ?error
sp@
here lit 128 +
u< lit 7 ?error
;s""")

word("INTERPRET:interpret", """:
-find 0branch 30
	state @ < 0branch 10
		cfa ,
	branch 6
		cfa execute
	?stack
branch 28
	here number
	dpl @ 1+ 0branch 8
		dliteral
	branch 6
		drop literal
	?stack
branch -62
""")

word("IMMEDIATE:immediate", ": latest lit 64 toggle ;s")

# This creates a word that represents vocabulary.
# This word looks like it has double header 1. word header 2. vocabulary header (with " " for a name).
# One links to previous word, second one to previous vocabulary.
# LFA of a first word in newly created vocabulary points to this "inner word" of the parent vocabulary.
# "inner word" LFA is always updated to point to the last word of the vocabulary (and voc-link points there).
# So when finding a word and switching from child vocabulary to parent vocabulary it starts wuith last word
# of a parent vocabulary.
#  current - where new definitions go
#  context - where to look-up words for execution
#  voc-link - points to previous vocabulary definition
# voc-link gives linear history of created vocabularies,
# while vocabularies generally can create tree structure.
word("VOCABULARY:vocabulary", """:
<builds
lit 2081h ,
current @ cfa ,
here
voc-link @ ,
voc-link !
does>
{X_VOCABULARY:}
2+ context !
;s""")

# newly defined vocabulary has "current @ cfa" stored to vocabulary last defined word
#  (maybe just "current @ 2-" ...)
#  (when new word is defined "current @ 2-" points to this second level header that looks like " ")
# newly defined vocabulary has "voc-link @" in place of as a link to previous voc

word("DEFINITIONS:definitions", ": context @ current ! ;s")
word("OPENBRKT:(", ": lit 41 word drop ;s", immediate=True)
word("BCKSLASH:\\", ": 0 word drop ;s", immediate=True)

# This is the last thing ever executed and is the interpreter
# outer loop. This NEVER quits.
word("QUIT:quit", """:
{.if BLOCKS}
0 blk !
{.endif}
[
	rp!
	cr
	query
	interpret
	state @ 0= 0branch 7
		(.") {"OK"}
branch -25
""")

word("ABORT:abort", ": uabort @ execute ;s")
word("FREE:free", ": sp@ here - ;s")

word("CF_UABORT:", """:
sp!
decimal
?stack
cr (.") {"* Z80 FORTH *"}
cr free lit 5 .r (.") {" bytes free"}
VOCAB_BASE+(C_FORTH-W_FORTH)
{	; ^ C_FORTH in RAM}
definitions
quit
""", no_head=True)

# this resets: s0 r0 tib width warning fence
# dp and voc-link are not touched - otherwise system gets inconsistent
word("WARM:warm", """:
lit INIT_TABLE
lit S0
lit 12
cmove
abort""")

verbatim("""
X_COLD:
	LD	HL,START_TABLE		;Copy table to ram
	LD	DE,START_TABLE_RAM	;Where the table's going
	LD	BC,START_TABLE_END-START_TABLE	;Bytes to copy
	LDIR				;
	LD	HL,W_FORTH		; Copy last word to ram -- need to update when creating a NEW WORD
	LD	DE,VOCAB_BASE		; Where it's going
	LD	BC,W_FORTH_END-W_FORTH	;Bytes to copy
	LDIR				;
	LD	BC,FIRSTWORD		;BC to first forth word
	LD	HL,(INIT_TABLE)		;Get stack pointer
	LD	SP,HL			;Set it
	LD	IX,SYSTEM		;Set return stack pointer
	JP	NEXT

FIRSTWORD:
	.WORD	P_COLD
""")

# this resets: s0 r0 tib width warning fence dp voc-link
word("COLD:cold", """X_COLD
{P_COLD:} X_COLON
0 offset !
lit INIT_TABLE
lit S0
lit START_TABLE-INIT_TABLE
cmove
abort""")

# Change single number to double
asm_word("SINGTODUB:s->d", """
	POP	DE			;Get number
	LD	HL,0000h		;Assume +ve extend
	LD	A,D			;Check sign
	AND	80h			;
	JR	Z,IS_POS		;Really +ve so jump
	DEC	HL			;Make -ve extension
IS_POS:
	JP	NEXTS2			;Save both & NEXT
""")

word("PLUSMINUS:+-", ": 0< 0branch 4 negate ;s") # negate if second arg < 0
word("DPLUSMINUS:d+-", ": 0< 0branch 4 dnegate ;s")
word("ABS:abs", ": dup +- ;s")
word("DABS:dabs", ": dup d+- ;s")
word("MIN:min", ": 2dup > 0branch 4 swap drop ;s")
word("MAX:max", ": 2dup < 0branch 4 swap drop ;s")
word("MTIMES:m*", ": 2dup xor >r abs swap abs u* r> d+- ;s") # multiply signed values
word("MDIV:m/", ": over >r >r dabs r@ abs u/mod r> r@ xor +- swap r> +- swap ;s") # ( d n - d/n )
word("TIMES:*", ": m* drop ;s") # ( n m - d )
word("DIVMOD:/mod", ": >r s->d r> m/ ;s")
word("DIV:/", ": /mod swap drop ;s")
word("MOD:mod", ": /mod drop ;s")
word("TIMESDIVMOD:*/mod", ": >r m* r> m/ ;s") # ( n m x - rem(n*m/x) floor(n*m/x) )
word("TIMESDIV:*/", ": */mod swap drop ;s") # ( n m x - floor(n*m/x) )
word("MDIVMOD:m/mod", ": >r 0 r@ u/mod r> swap >r u/mod r> ;s")

verbatim("""
.if BLOCKS
""")
word("CLINE:(line)", ": >r c/l b/buf */mod r> b/scr * + block + c/l ;s")
word("DOTLINE:.line", ": (line) -trailing type ;s")
verbatim("""
.endif
""")

word("MESSAGE:message", """:
{.if BLOCKS}
warning @ 0branch 30
	?dup 0branch 20
		lit 4
		offset @
		b/scr / - .line
		space
	branch 13
{.endif}
		(.") {"MSG # "} .
;s""")

asm_word("PORTIN:p@", """
	POP	DE			;Get port addr
	LD	HL,PAT+1		;Save in port in code
	LD	(HL),E			;
	CALL	PAT			;Call port in routine
	LD	L,A			;Save result
	LD	H,00h			;
	JP	NEXTS1			;Save & NEXT
""")

asm_word("PORTOUT:p!", """
	POP	DE			;Get port addr
	LD	HL,PST+1		;Save in port out code
	LD	(HL),E			;
	POP	HL			;
	LD	A,L			;Byte to A
	CALL	PST			;Call port out routine
	JP	NEXT
""")

verbatim("""
.if BLOCKS
""")
word("USE:use", "user USE-SYSTEM")
word("PREV:prev", "user PREV-SYSTEM")
word("PLUSBUF:+buf", "NEXT")
word("UPDATE:update", "NEXT")
word("EBUFFERS:empty-buffers", ": first limit over - erase ;s")
word("BUFFER:buffer", ": block ;s")
word("BLOCK:block", ": lit 40 mod offset @ + b/buf * first + ;s")
word("RW:r/w", ": ur/w @ execute ;s")
verbatim("""
.endif
""")
word("CF_URW:", ": drop drop drop ;s", no_head=True)

verbatim("""
.if BLOCKS
""")
word("FLUSH:flush", ": ;s")
verbatim("""
.endif
""")

word("DUMP:dump", """:
0 (do)
	cr
	dup 0 lit 5 d.r
	space
	lit 4 swap
	over 0 (do)
		dup c@ 3 .r 1+
	(loop) -12
	swap
(+loop) -44
drop
cr
;s""")

verbatim("""
.if BLOCKS
""")
word("LOAD:load", """:
blk @ >r
>in @ >r
0 >in !
b/scr * blk !
interpret
r> >in !
r> blk !
;s""")

word("NEXTSCREEN:-->", """:
?loading
0 >in !
b/scr blk @
over mod - blk +!
;s""", immediate=True)

verbatim("""
.endif
""")

word("TICK:'", ": -find 0= 0 ?error drop literal ;s")

word("FORGET:forget", """:
current @ context @ - lit 24 ?error
'
dup fence @
< lit 21 ?error
dup nfa dp !
lfa @ context @ !
;s""")

word("BACK:back", ": here - , ;s")
word("BEGIN:begin", ": ?comp here 1 ;s", immediate=True)
word("ENDIF:endif", ": ?comp 2 ?pairs here over - swap ! ;s", immediate=True)
word("THEN:then", ": endif ;s", immediate=True)
word("DO:do", ": compile (do) here 3 ;s", immediate=True)
word("LOOP:loop", ": 3 ?pairs compile (loop) back ;s", immediate=True)
word("PLUSLOOP:+loop", ": 3 ?pairs compile (+loop) back ;s", immediate=True)
word("UNTIL:until", ": 1 ?pairs compile 0branch back ;s", immediate=True)
word("END:end", ": until ;s", immediate=True)
word("AGAIN:again", ": 1 ?pairs compile branch back ;s", immediate=True)
word("REPEAT:repeat", ": >r >r again r> r> 2- endif ;s", immediate=True)
word("IF:if", ": compile 0branch here 0 , 2 ;s", immediate=True)
word("ELSE:else", ": 2 ?pairs compile branch here 0 , swap 2 endif 2 ;s", immediate=True)
word("WHILE:while", ": if 2+ ;s", immediate=True)
word("SPACES:spaces", ": 0 max ?dup 0branch 12 0 (do) space (loop) -4 ;s")

word("RECURSE:recurse", ": ?comp latest pfa cfa , ;s", immediate=True)

word("LESSHARP:<#", ": pad hld ! ;s")
word("SHARPGT:#>", ": drop drop hld @ pad over - ;s")
word("SIGN:sign", ": rot 0< 0branch 8 lit 45 hold ;s")
word("SHARP:#", """:
	base @
	m/mod rot
	lit 9 over < 0branch 8
		lit 7 +
	lit 48 +
	hold
;s""")
word("SHARPS:#s", ": # 2dup or 0= 0branch -10 ;s")
word("DDOTR:d.r", ": >r swap over dabs <# #s sign #> r> over - spaces type ;s")
word("DOTR:.r", ": >r s->d r> d.r ;s")
word("DDOT:d.", ": 0 d.r space ;s")
word("DOT:.", ": s->d d. ;s")
word("QUESTION:?", ": @ . ;s")
word("UDOT:u.", ": 0 d. ;s")
word("WORDS:words", """:
context @ @
cr
	dup id.
	pfa lfa @
	dup 0= ?terminal or
0branch -20
drop
cr
;s""")

word("VOCS:vocs", """:
voc-link @ 
	dup lit 11 - lit -1 traverse id.
	@
dup 0= 0branch -24
drop
;s""")

verbatim("""
.if BLOCKS
""")

word("LIST:list", """:
base @ swap
decimal
cr
dup scr !
(.") {"SCR # "} .
lit 16 0 (do)
	cr
	i lit 0003h .r
	space
	i scr @ .line
	?terminal 0branch 4
		leave
(loop) -30
cr
base !
;s""")

word("INDEX:index", """:
1+ swap (do)
	cr
	i lit 0003h .r
	space
	0 i .line
	?terminal 0branch 4
		leave (loop)
-28
cr
;s""")

verbatim("""
.endif

.if INTERRUPTS
""")
def_word("INT:;int", ": ?csp compile X_INT [ smudge ;s", """
X_INT:
	.WORD	2+$			;Vector to code
	LD	HL,INTFLAG
	RES	6,(HL)
	EI
	JP	X_STOP
""", immediate=True)

word("INTFLAG:intflag", "user INTFLAG-SYSTEM")
word("INTVECT:intvect", "user INTVECT-SYSTEM")

verbatim("""
.endif
""")

word("CPU:.cpu", ': (.") {"Z80 "} ;s')

word("2SWAP:2swap", ": rot >r rot r> ;s")
word("2OVER:2over", ": >r >r 2dup r> r> 2swap ;s")

word("EXIT:exit", ";s")

# roll with negative value is dangerous
word("ROLL:roll", """:
dup 0 > 0branch 44
	dup >r
	pick r>
	0 swap (do)
		sp@ i dup + +
		dup 2- @
		swap !
	lit -1 (+loop) -26
drop
;s""")

word("DEPTH:depth", ": s0 @ sp@ - 2 / 1- ;s")

word("DLESSTHAN:d<", """:
rot
2dup = 0branch 10
	2drop u<
branch 8
	2swap 2drop >
;s""")

word("0GREATER:0>", ": 0 > ;s")

word("DOTS:.s", """:
cr
depth 0branch 32
	sp@ 2-
	s0 @ 2-
	(do)
		i @ .
	lit -2 (+loop) -12
branch 17
	(.") {"STACK EMPTY "}
;s""")

# e.g.: code nop 0 , next end-code
word("CODE:code", ": ?exec create !csp ;s")
word("ENDCODE:end-code", ": current @ context ! ?exec ?csp smudge ;s")
word("NEXT:next", ": lit 195 c, lit NEXT , ;s", immediate=True)

verbatim("""
.if BLOCKS
""")
word("LLOAD:lload", """:
block
lit 0000h
	dup 0branch 40
		dup lit 13 = 0branch 18
			drop c/l + c/l negate and
		branch 6
			over !
		1+
	branch 4
		drop
	key
	dup lit 26 =
0branch -58
drop drop
;s""")
verbatim("""
.endif

.if SYSDUMP_LOAD
""")

word("HDUMPS:hdumps", "user HDUMPS-SYSTEM")
word("H1HDOT:1h.", ": 0 <# # # #> type ;s")
word("H2HDOT:2h.", ": bswap 0 <# # # # # #> type ;s")
word("H4HDOT:4h.", ": bswap swap bswap swap <# # # # # # # # # #> type ;s")
word("H2SUM:2sum", ": dup bswap + ;s")
word("H4SUM:4sum", ": over bswap over bswap + + + ;s")

word("CHDUMP:(hdump)", """:
dup 3 > 0branch 38
	swap dup 2@ 4h.
	dup 2@ 4sum hdumps +!
	lit 4 +
	swap lit 4 -
branch -44
dup 1 > 0branch 26
	swap dup @ 2h.
	dup @ 2sum hdumps +!
	2+
	swap 2-
dup 0> 0branch 22
	swap dup c@ 1h.
	dup c@ hdumps +!
	1+
	swap
drop
;s""")

word("HDUMP:hdump", """:
base @ rot rot
hex
cr
dup 0> 0branch 84
	lit 58 emit
	dup lit 16 min
	swap over
	- swap
	dup 1h.
	dup hdumps !
	rot dup bswap 2h.
	dup 2sum hdumps +!
	0 1h.
	swap
	(hdump)
	swap
	lit 256 hdumps @ lit 255 and - 1h.
	cr
branch -88
2drop
base !
;s""")

word("HEOF:heof", """:
lit 58 emit
lit 256 0 4h.
lit -1 1h.
cr
;s""")

word("SYSDUMP:sysdump", """:
lit VOCAB_BASE here over - hdump
dp lit 4 hdump
base @
hex
heof
base !
;s""")

asm_word("HLOAD:hload","""
	PUSH	BC
        ; Load Intel HEX into program memory.
        ; uses  : af, bc, de, hl
        ; (C) feilipu
HLD_WAIT_COLON:
        rst 10h                 ; Rx byte in A
        cp ':'                  ; wait for ':'
        jp NZ,HLD_WAIT_COLON
        ld c,0                  ; reset C to compute checksum
        call HLD_READ_BYTE      ; read byte count
        ld b,a                  ; store it in B
        call HLD_READ_BYTE      ; read upper byte of address
        ld d,a                  ; store in D
        call HLD_READ_BYTE      ; read lower byte of address
        ld e,a                  ; store in E
        call HLD_READ_BYTE      ; read record type
        dec a                   ; check if record type is 01 (end of file)
        jp Z,HLD_END_LOAD
        inc a                   ; check if record type is 00 (data)
        jp NZ,TMERR             ; if not, type mismatch error
HLD_READ_DATA:
        call HLD_READ_BYTE
        ld (de),a              ; write the byte at the RAM address, increment
        inc de
        djnz HLD_READ_DATA      ; if b non zero, loop to get more data
HLD_READ_CHKSUM:
        call HLD_READ_BYTE      ; read checksum, but we don't need to keep it
        ld a,c                  ; lower byte of C checksum should be 0
        or a
        jp NZ,HXERR             ; non zero, we have an issue
        jp HLD_WAIT_COLON
HLD_END_LOAD:
        call HLD_READ_BYTE      ; read checksum, but we don't need to keep it
        ld a,c                  ; lower byte of C checksum should be 0
        or a
        jp NZ,HXERR             ; non zero, we have an issue
	POP	BC
	LD	HL,0
	JP	NEXTS1
TMERR:
HXERR:
	POP	BC
	LD	H,0
	LD	L,A
	JP	NEXTS1

HLD_READ_BYTE:                  ; returns byte in A, checksum in C
        call HLD_READ_NIBBLE    ; read the first nibble
        rlca                    ; shift it left by 4 bits
        rlca
        rlca
        rlca
        ld l,a                  ; temporarily store the first nibble in L
        call HLD_READ_NIBBLE    ; get the second (low) nibble
        or l                    ; assemble two nibbles into one byte in A
        ld l,a                  ; put assembled byte back into L
        add a,c                 ; add the byte read to C (for checksum)
        ld c,a
        ld a,l
        ret                     ; return the byte read in A (L = char received too)

HLD_READ_NIBBLE:
        rst 10h                 ; Rx byte in A
        sub '0'
        cp 10
        ret C                   ; if A<10 just return
        sub 7                   ; else subtract 'A'-'0' (17) and add 10
        ret
""")

word("SYSLOAD:sysload", ": cr hload 0branch 10 lit 69 emit . cr warm ;s")
verbatim("""
.endif
""")

# voc-link links to E_FORTH (but copied to RAM)
# note initial context and current is initialized by "uabort", called from "abort", called from "warm"
# forth is the last word and is copied to RAM
word("FORTH:forth", """does> X_VOCABULARY
2081h VOCAB_BASE
{E_FORTH:} 0000h
{W_FORTH_END:}
""", immediate=True)

verbatim("""
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
	CALL	CHR_WR			;User output routine
	JP	NEXT			;

CF_UCR:					;CR output
	.WORD	2+$			;Vector to code
	LD	A, CR			;Carrage return
	CALL	CHR_WR			;User output routine
	LD	A, LF			;Line feed
	CALL	CHR_WR			;User output routine
	JP	NEXT			;Next

CF_UQTERMINAL:				;Test for user break
	.WORD	2+$			;Vector to code
	CALL	BREAKKEY		;User break test routine
	LD	H,00h			;Clear H
	LD	L,A			;Result in L
	JP	NEXTS1			;Store it & Next

;==============================================================================
; Serial I/O routines
; Rachel - To use INT32K.ASM
;==============================================================================

CHR_RD:					;Character in
	; This call preserves registers with exception of AF (and below stack pointer)
	RST 10h
	RET

BREAKKEY:
	LD	A,(serRxBufUsed)
	RET	Z			;Nothing in buffer, so no break
	LD	HL,(serRxInPtr)		;Get the pointer to where we poke
	DEC	L			;This should work with BASIC, with ROM not necessary
	LD	A,ESC
	SUB	(HL)			;Was ESC last char?
	JR	Z,BREAK
	XOR	A
	RET				;No break
BREAK:
	INC	A			;Make A non-zero
	RET				;Return

CHR_WR:					;Character out
	; This call preserves registers with exception of AF (and below stack pointer)
	RST 08h
	RET

.section .bss
;==============================================================================
;		.ORG	0FE00h	        ;Set up system variable addresses
;==============================================================================
SYSTEM:					;Start of scratch pad area
		.space	6		;User bytes -- uninitialized
; following 6 words are initialized by "warm" and "cold"
S0:		.space	2		;Initial value of the data stack pointer (DATA_STACK)
R0:		.space	2		;Initial value of the return stack pointer (SYSTEM)
TIB:		.space	2		;Address of the terminal input buffer (DATA_STACK)
WIDTH:		.space	2		;Number of letters saved in names (31)
WARNING:	.space	2		;Error message control number (0)
FENCE:		.space	2		;Dictionary FORGET protection point (VOCAB_BASE)
; following 2 words are initialized only by "cold"
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
START_TABLE_RAM:
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
HDUMPS:		.space	2		;Checksum for hdump
		.space	30		;User bytes

;==============================================================================""")

import sys

if len(sys.argv) == 1:
	for c in chunks:
		(c[0])(*c[1:])

elif sys.argv[1] == 'nodes':
	print("Name,Type")
	for c in chunks:
		t = None
		if c[0] == print_word:
			t = "code"
		if c[0] == print_asm_word:
			t = "asm"
		elif c[0] == print_def_word:
			t = "def"
		if t:
			print(f"{c[1]},{t}")

elif sys.argv[1] in ['edges','essential','gv']:
	edges = {}
	for c in chunks:
		t = None
		if c[0] == print_word:
			t = "code"
		if c[0] == print_asm_word:
			t = "asm"
		elif c[0] == print_def_word:
			t = "def"
		if t not in ["code","def"]:
			continue
		[label, name, words] = c[1:4]
		curly = CurlySubst()
		words = re.sub(curly.pattern, curly.eval_curly, words.strip())
		ws = re.split(r'\s+', words.strip())
		out = set()
		for w in ws[1:]:
			n = find('',w)
			if n != w:
				out.add(n[1:])
		edges[label] = out

	if sys.argv[1] == 'edges':
		print("Source,Target")
		for label, out in edges.items():
			for o in out:
				print(f"{label},{o}")

	if sys.argv[1] == 'gv':
		visited = set()
		def descendants(words):
			for w in words:
				if w not in visited:
					visited.add(w)
					if w in edges:
						descendants(list(edges[w]))
		descendants(['WARM','COLD','QUIT'])

		print("digraph G {")
		for label, out in edges.items():
			if (label in visited) and (len(out) > 0):
				print(f"{label} -> {{{' '.join(out)}}}")
		print("}")

	elif sys.argv[1] == 'essential':
		visited = set()
		def descendants(words):
			for w in words:
				if w not in visited:
					visited.add(w)
					if w in edges:
						descendants(list(edges[w]))
		descendants(['WARM','COLD','QUIT'])
		for w in visited:
			print(w)
