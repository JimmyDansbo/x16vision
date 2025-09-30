.include "x16.inc"

.import __XVKIT_LOWRAM_SIZE__

X16VISION_VERSION = $0001

.segment "JUMPTABLE"
	jmp	xv_initialize	; $A000
	jmp	xv_setisr	; $A003
	jmp	xv_clearisr	; $A006

.segment "XVKITBSS"
_XV_BSS_START = *
; Lowram address provided by the user to copy ISR routine to.
lowram_addr:	.res 2
; Object handled by library are in a linked list, this points to the start of the list.
init_obj_bank:	.res 1
init_obj_addr:	.res 2

_XV_BSS_END = *

.segment "XVKITLIB"
.proc	xv_setisr: near
	nop

	lda	#$EA		; NOP
	sta	xv_clearisr	; Ungate xv_clearisr
	lda	#$60		; RTS
	sta	xv_setisr	; Gate xv_setisr

	; Save ZP ptr to stack
	lda	X16_PTR_0
	pha
	lda	X16_PTR_0+1
	pha

	; Set ZP ptr to start of lowram isr
	lda	lowram_addr
	sta	X16_PTR_0
	lda	lowram_addr+1
	sta	X16_PTR_0+1
	; Set Y to offset of _old_isr
	ldy	#(_end_isr-_isr-2)

	; Save old interrupt vector
	lda	X16_Vector_IRQ
	sta	(X16_PTR_0),y
	iny
	lda	X16_Vector_IRQ+1
	sta	(X16_PTR_0),y

	; Restore ZP ptr from stack
	pla
	sta	X16_PTR_0+1
	pla
	sta	X16_PTR_0

	; Tell VERA that we want VSYNC interrupts (should already be enabled)
	lda	Vera_Reg_IEN
	ora	#$01
	sta	Vera_Reg_IEN

	; Install new interrupt vector
	sei	; Disable interrupts
	lda	lowram_addr
	sta	X16_Vector_IRQ
	lda	lowram_addr+1
	sta	X16_Vector_IRQ+1
	cli	; Enable interrupts
	rts
.endproc

.proc	xv_clearisr: near
	rts

	lda	#$EA		; NOP
	sta	xv_setisr	; Ungate xv_setisr
	lda	#$60		; RTS
	sta	xv_clearisr	; Gate xv_clearisr

	; Save ZP ptr to stack
	lda	X16_PTR_0
	pha
	lda	X16_PTR_0+1
	pha

	; Set ZP ptr to start of lowram isr
	lda	lowram_addr
	sta	X16_PTR_0
	lda	lowram_addr+1
	sta	X16_PTR_0+1
	; Set Y to offset of _old_isr
	ldy	#(_end_isr-_isr-2)
	
	sei	; Disable interrupts
	lda	(X16_PTR_0),y
	sta	X16_Vector_IRQ
	iny
	lda	(X16_PTR_0),y
	sta	X16_Vector_IRQ+1
	cli	; Enable interrupts

	; Restore ZP ptr from stack
	pla
	sta	X16_PTR_0+1
	pla
	sta	X16_PTR_0

	rts
.endproc

.proc	xv_initialize: near
	; Save low byte of lowram address
	sta	lowram_addr
	; Save ZP ptr to stack
	lda	X16_PTR_0
	pha
	lda	X16_PTR_0+1
	pha
	; Write low byte of lowram address to ZP ptr
	lda	lowram_addr
	sta	X16_PTR_0
	; Save high byte of lowram address and write it to ZP ptr
	stx	lowram_addr+1
	stx	X16_PTR_0+1
	; Save current RAM bank, this is where x16vision library is loaded
	lda	X16_RAMBank_Reg
	sta	ISRBANK

	; Copy ISR routine to low ram
	phy	; Preserve Y
	ldy	#(_end_isr-_isr-1) ; Size of ISR routine
@loop:
	lda	_isr,y
	sta	(X16_PTR_0),y
	dey
	bpl	@loop
	ply	; Restore Y

	; Restore ZP ptr
	pla
	sta	X16_PTR_0+1
	pla
	sta	X16_PTR_0
	rts
.endproc

.proc	xv_tick: near
	rts
.endproc

.segment "XVKIT_LOWRAM"
_isr:
	; Check if interrupt is VSYNC
	lda	Vera_Reg_ISR
	and	#$01
	beq	_old_isr	; If not VSYNC, skip this ISR
	; Save current RAM bank
	lda	X16_RAMBank_Reg
	pha
	; Set RAM bank where x16vision library is loaded
	lda	#$FF	; Modified to correct bank = _isr+11
ISRBANK = * - 1
	sta	X16_RAMBank_Reg
	; Handle tick
	jsr	xv_tick
	; Restore RAM Bank
	pla
	sta	X16_RAMBank_Reg
_old_isr:
	jmp	$FFFF
_end_isr:

.assert __XVKIT_LOWRAM_SIZE__ <= 255, error, "XVKIT_LOWRAM segment may not be larger than 255 bytes"