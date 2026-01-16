.include "x16.inc"
.include "x16vision.inc"
SKIPIMPORT=1
.include "memman.inc"

.import __LOWRAM_SIZE__, __LOWRAM_SIZE__
.export init_lowram, set_banked_isr, clear_banked_isr, check_space, allocate_space

.segment "MEMMANBSS"
lowram_addr:	.res	2
orig_isr:	.res	2
scratch:	.res	2

REM_SPACE=$A000
FREE_ADDR=$A002

_isr_bank=4
_isr_addr=8
_isr_orig=14

.segment "MEMMAN"
; Internal jump table into lowram functions
; The bank-load and store functions use LD_ST_BANK_PTR for the address and X for bank
lda_bank:		; Return byte in A					X=bank, Y=offset
	jmp	$0000
lday_bank:		; Return lowbyte in A, highbyte in Y			X=bank
	jmp	$0000
ldyxa_bank:		; Return lowbyte in Y, midbyte in X , highbyte in A	X=bank
	jmp	$0000
sta_bank:		; Store value in A					A=val, X=bank, Y=offset
	jmp	$0000
stay_bank:		; Store A to lowbyte, Y to highbyte			X=bank
	jmp	$0000

;*****************************************************************************
; Allocate the requested number of bytes if they are available
;=============================================================================
; Inputs:	.A = number of bytes
; 		.X = bank
; Output:	.A & .Y = low- and high-byte of address of allocated memory
;		.C = set if memory is available, otherwise clear
;-----------------------------------------------------------------------------
; Preserves:	.X
; Uses:		LD_ST_BANK_PTR
;*****************************************************************************
.proc allocate_space: near
	jsr	check_space
	bcc	@end
	; Memory is available here. Calculate the new free space and pointer
	pha			; Save the number of bytes being requested
	; Set ZP pointer to FREE_ADDR
	lda	#<FREE_ADDR
	sta	LD_ST_BANK_PTR
	lda	#>FREE_ADDR
	sta	LD_ST_BANK_PTR+1
	jsr	lday_bank	; Read next available address from bank
	sta	scratch
	sty	scratch+1

	pla			; Restore number of bytes being requested

	; Save address on stack as this needs to be returned to caller
	phy		; high-byte
	ldy	scratch
	phy		; low-byte


	clc			; Calculate new next available address
	adc	scratch
	sta	scratch
	lda	#0
	adc	scratch+1
	sta	scratch+1

	; Calculate new free space after allocation of memory
	lda	#<X16_ROM_Window
	sec
	sbc	scratch
	pha
	lda	#>X16_ROM_Window
	sbc	scratch+1
	tay
	stz	LD_ST_BANK_PTR
	pla
	jsr	stay_bank
	lda	#<FREE_ADDR
	sta	LD_ST_BANK_PTR
	lda	scratch
	ldy	scratch+1
	jsr	stay_bank
	pla
	ply
	sec
@end:	rts
.endproc

;*****************************************************************************
; Check if requested amount of space is available in the specified bank
; Maximum request size is 255 bytes
;=============================================================================
; Inputs:	.A = number of bytes 
;		.X = bank
; Output:	Carry set if the memory is available, otherwise clear
;-----------------------------------------------------------------------------
; Uses:		LD_ST_BANK_PTR
; Preserves:	.A & .X
;*****************************************************************************
.proc check_space: near
	pha			; Save the number of bytes being requested
	lda	#>REM_SPACE	; Set address $A000 in ZP pointer
	stz	LD_ST_BANK_PTR
	sta	LD_ST_BANK_PTR+1
	jsr	lday_bank	; Read available memory from bank
	sta	scratch
	sty	scratch+1
	pla			; Restore bytes requested
	ldy	scratch+1	; Check high-byte
	bne	@good
	; Here, we know that high-byte is 0
	pha			; Save on stack again
	sta	scratch+1	; Store bytes requested to scratch and load
	lda	scratch		; available bytes into A for CMP
	cmp	scratch+1
	pla			; Restore bytes requested
	rts	; Exit function with C set if mem avail otherwise clear
@good:	sec
	rts
.endproc

;*****************************************************************************
; Restores the interrupt vector to the one saved by set_banked_isr
;=============================================================================
; No inputs
;-----------------------------------------------------------------------------
; Uses: .A
;*****************************************************************************
.proc clear_banked_isr: near
	sei	; Disable interrupts
	lda	orig_isr
	sta	X16_Vector_IRQ
	lda	orig_isr+1
	sta	X16_Vector_IRQ+1
	cli	; Enable interrupts
	rts
.endproc

;*****************************************************************************
; Updates the ISR in lowram with the correct bank and address of the actual
; ISR, then installs the lowram ISR and ensures it calls the original ISR
;=============================================================================
; Input:	A and X are low- and high-byte of banked ISR
;		Y is ZP address to use as a pointer
;		 - low-byte of the ZP address should contain the bank of ISR
;-----------------------------------------------------------------------------
;*****************************************************************************
.proc set_banked_isr: near
@ZP=$42
	; Self modify to use correct ZP pointer
	sty	@zp0+1
	sty	@zp1+1
	sty	@zp3+1
	sty	@zp4+1
	sty	@zp5+1
	sty	@zp6+1
	sty	@zp7+1
	iny
	sty	@zp2+1
	pha	; Save low-byte of ISR on stack as .A needs to be used
	; Get RAM bank of ISR (stored in low-byte of ZP pointer)
@zp0:	lda	@ZP
	tay	; Store RAM bank in .Y
	; Store lowram_addr in ZP pointer
	lda	lowram_addr
@zp1:	sta	@ZP
	lda	lowram_addr+1
@zp2:	sta	@ZP+1
	pla	; Restore low-byte of ISR from stack
	phy	; Preserve bank of ISR on stack
	; Store low-byte of address for banked ISR
	ldy	#_isr_addr
@zp3:	sta	(@ZP),y
	; Store high-byte of address for banked ISR
	iny
	txa
@zp4:	sta	(@ZP),y
	; Store bank# of banked ISR
	ldy	#_isr_bank
	pla	; Restore bank of ISR from stack
@zp5:	sta	(@ZP),y
	; Save original interrupt vector
	ldy	#_isr_orig
	lda	X16_Vector_IRQ
	sta	orig_isr
@zp6:	sta	(@ZP),y
	iny
	lda	X16_Vector_IRQ+1
	sta	orig_isr+1
@zp7:	sta	(@ZP),y
	; Install new interrupt vector
	sei	; Disable interrupts
	lda	lowram_addr
	sta	X16_Vector_IRQ
	lda	lowram_addr+1
	sta	X16_Vector_IRQ+1
	cli	; Enable interrupts
	rts
.endproc

;*****************************************************************************
; Copies functions to lowram and updates jumptable 
;=============================================================================
; Arguments: A and X are low- and high-byte of lowram address reserved for
;            the x16vision library
;	     Y is the ZP address to use as pointer
;-----------------------------------------------------------------------------
; 
;*****************************************************************************
.proc init_lowram: near
@ZP=$42
	; Save the lowram address
	sta	lowram_addr
	stx	lowram_addr+1

	; Selfmodify to use correct ZP pointer
	sty	@zp0+1
	sty	@zp2+1
	sty	@zp3+1
	iny
	sty	@zp1+1
@zp0:	sta	@ZP
@zp1:	stx	@ZP+1
	; Copy routines to lowram
	ldy	#(_end_lowram-_isr-1)	; Size of lowram segment
@loop:	lda	_isr,y
@zp2:	sta	(@ZP),y
	dey
	bne	@loop
	lda	_isr
@zp3:	sta	(@ZP)
	; Update jumptable
	lda	#(_lda_bank-_isr)	; Offset of _lda_bank
	clc
	adc	lowram_addr		; Add offset to lowram address
	sta	lda_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	lda_bank+2
	lda	#(_lday_bank-_isr)
	clc
	adc	lowram_addr
	sta	lday_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	lday_bank+2
	lda	#(_ldyxa_bank-_isr)
	clc
	adc	lowram_addr
	sta	ldyxa_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	ldyxa_bank+2
	lda	#(_sta_bank-_isr)
	clc
	adc	lowram_addr
	sta	sta_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	sta_bank+2
	lda	#(_stay_bank-_isr)
	clc
	adc	lowram_addr
	sta	stay_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	stay_bank+2
	rts
.endproc

.segment "LOWRAM"
;*****************************************************************************
; The current RAM bank is saved, RAM bank set correctly and a jsr to ISR
; function is performed before continuing on to previous interrupt handler
;=============================================================================
; ROM has already preserved registers and previous interrupt handler should
; ensure they are restored. RAM bank is restored before call to original
; interrupt handler
;*****************************************************************************
_isr:
	; Save current RAM bank
	lda	X16_RAMBank_Reg
	pha
	; Set new RAM bank
	lda	#$FF	; Must be modified after copy to lowram (offset=4)
	sta	X16_RAMBank_Reg
	; Call banked ISR
	jsr	$FFFF	; Must be modified after copy to lowram (offset=8,9)
	; Restore RAM bank
	pla
	sta	X16_RAMBank_Reg
	; Continue to original ISR
	jmp	$FFFF	; Must be modified after copy to lowram (offset=14,15)

;*****************************************************************************
; Load a byte from banked RAM pointed to by ZP pointer LD_ST_BANK_PTR in bank
; specified by the value in .X
;=============================================================================
; Arguments: LD_ST_BANK_PTR pointer to the memory address that should be read
;            .X holds the RAM bank to read from
;	     .Y holds offset from ZP pointer
;-----------------------------------------------------------------------------
; Preserves: .X, .Y, LD_ST_BANK_PTR and the RAM bank before call
; Returns: .A contains the value read from memory
;*****************************************************************************
_lda_bank:
	phx				; Preserve bank
	phy				; Preserve offset
	ldy	X16_RAMBank_Reg		; Save current RAM bank
	stx	X16_RAMBank_Reg		; Set new RAM bank
	phy				; Move original RAM bank from Y to X through stack
	plx
	ply				; Pull offset from stack
@ptr:	lda	(LD_ST_BANK_PTR),y	; Load value from address pointed to by ZP pointer
	stx	X16_RAMBank_Reg		; Restore original RAM bank
	plx				; Restore RAM bank from caller
	rts

;*****************************************************************************
; Load two bytes from banked RAM pointed to by ZP pointer LD_ST_BANK_PTR in
; bank specified by the value in .X
;=============================================================================
; Arguments: LD_ST_BANK_PTR, pointer to the memory address that should be read
;            .X holds the RAM bank to read from
;-----------------------------------------------------------------------------
; Preserves: X & LD_ST_BANK_PTR
; Returns: .A contains value from ZP pointer, .Y from ZP pointer + 1
;*****************************************************************************
_lday_bank:
	phx			; Preserve X
	ldy	X16_RAMBank_Reg	; Save original RAM bank
	stx	X16_RAMBank_Reg	; Switch RAM bank
	phy			; Move original RAM bank through stack to X
	plx
	ldy	#1		; Read highbyte into Y
	lda	(LD_ST_BANK_PTR),y
	tay
	lda	(LD_ST_BANK_PTR); Read lowbyte into A
	stx	X16_RAMBank_Reg	; Restore original RAM bank
	plx			; Restore X
	rts

;*****************************************************************************
; Load three bytes from banked RAM pointed to by ZP pointer LD_ST_BANK_PTR in00
; bank specified by the value in .X
;=============================================================================
; Arguments: LD_ST_BANK_PTR, pointer to the memory address that should be read
;            .X holds the RAM bank to read from
;-----------------------------------------------------------------------------
; Preserves: LD_ST_BANK_PTR and the RAM bank before call
; Returns: .Y=ZP pointer, .X=ZP pointer+1, .A=ZP pointer+2
;*****************************************************************************
_ldyxa_bank:
	ldy	X16_RAMBank_Reg		; Save current RAM bank
	stx	X16_RAMBank_Reg		; Set new RAM bank
	lda	(LD_ST_BANK_PTR)	; Load value from address pointed to by ZP pointer
	pha				; Save value to stack
	phy				; Save RAM bank to stack
	ldy	#1			; Initialize .Y for reading value through ZP pointer
	lda	(LD_ST_BANK_PTR),y	; Load next value from address pointed to by ZP pointer with .Y
	tax				; Store value in .X
	iny
	lda	(LD_ST_BANK_PTR),y	; Load next value from address pointed to by ZP pointer with .Y
	ply				; Restore original RAM bank
	sty	X16_RAMBank_Reg
	ply				; Get first read value from stack
	rts

;*****************************************************************************
; Store a byte to banked RAM pointed to by ZP pointer LD_ST_BANK_PTR in bank
; specified by the value in .X
;=============================================================================
; Arguments: LD_ST_BANK_PTR, pointer to memory address that should be written
;            .X holds the RAM bank to write to
;	     .A holds the value to write
;	     .Y holds the offset to the ZP pointer to write to
;-----------------------------------------------------------------------------
; Preserves: .A, .X, .Y, LD_ST_BANK_PTR and the RAM bank before call
; Returns: nothing
;*****************************************************************************
_sta_bank:
	phx				; Preserve .X 
	pha				; Preserve values to write
	lda	X16_RAMBank_Reg		; Save current RAM bank
	stx	X16_RAMBank_Reg		; Set new RAM bank
	tax				; Move original RAM bank to .X
	pla				; Restore value to write
	sta	(LD_ST_BANK_PTR),y	; Store value in .A to address pointed to by ZP pointer
	stx	X16_RAMBank_Reg		; Restore RAM bank
	plx				; Restore .X
	rts

;*****************************************************************************
; Store two bytes to banked RAM pointed to by ZP pointer LD_ST_BANK_PTR in
; bank specified by the value in .X
;=============================================================================
; Arguments: LD_ST_BANK_PTR, pointer to memory address that should be written
;            .X holds the RAM bank to write to
;	     .A & .Y holds the values to write in that order (low-high)
;-----------------------------------------------------------------------------
; Preserves: A, X, Y, LD_ST_BANK_PTR & The RAM bank before call
; Returns: nothing
;*****************************************************************************
_stay_bank:
	pha				; Preserve A register
	pha				; Preserve low byte
	lda	X16_RAMBank_Reg		; Save current RAM bank in .A
	stx	X16_RAMBank_Reg		; Set the new RAM bank
	plx				; Pull .low byte from stack and store it in .X temporarily
	pha				; Push original RAM bank to stack
	txa				; Move low byte from .X back to .A
	sta	(LD_ST_BANK_PTR)	; Store low byte to address pointed to by ZP pointer
	tya				; Store high byte to address pointed to by ZP pointer with .Y added
	ldy	#1
	sta	(LD_ST_BANK_PTR),y
	tay				; Restore high byte to Y
	ldx	X16_RAMBank_Reg		; Restore RAM bank value from the call
	pla				; Restore RAM bank to original
	sta	X16_RAMBank_Reg
	pla				; Restore A register
	rts

_end_lowram:

.assert __LOWRAM_SIZE__ <= 255, error, "LOWRAM segment may not be larger than 255 bytes"
