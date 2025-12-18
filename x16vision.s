.include "x16.inc"
.include "macros.inc"

.import __XVKIT_LOWRAM_SIZE__, __XVKITBSS_SIZE__, __XVKITBSS_LOAD__

X16VISION_VERSION = $0001

.segment "JUMPTABLE"
	jmp	xv_initialize	; $A000
	jmp	xv_setisr	; $A003
	jmp	xv_clearisr	; $A006
	jmp	xv_desktop	; $A009

; Internal jump table into lowram functions
lda_bank:
	jmp	$0000
ldxa_bank:
	jmp	$0000
ldyxa_bank:
	jmp	$0000
sta_bank:
	jmp	$0000
stay_bank:
	jmp	$0000

.segment "XVKITBSS"

; Lowram address provided by the user to copy ISR routine to.
lowram_addr:	.res 2
; Object handled by library are in a linked list, this points to the start of the list.
init_obj_addr:	.res 2
init_obj_bank:	.res 1
; This points to the latest object for ease of adding objects.
last_obj_addr:	.res 2
last_obj_bank:	.res 1
; Space reserved for saving ZP pointer before using it
preserve_ptr:	.res 2
; Variable holding amount of free space in the bank
rem_space:	.res 2

START_OF_LINKED_LIST = *
.segment "XVKITLIB"
OP_NOP=$EA	; Opcode for NOP
OP_RTS=$60	; Opcode for RTS

;*****************************************************************************
;=============================================================================
;*****************************************************************************
.proc	xv_tick: near
	.byte $db
	; Check if address is all 0s
	lda	init_obj_addr
	ora	init_obj_addr+1
	beq	@end
	; Here we have an address for an object
	SAVE_PTR X16_PTR_0

	RESTORE_PTR X16_PTR_0
@end:	rts
.endproc

;*****************************************************************************
; Enable the ISR routine, but first ensure that it jumps to the original
; interrupt routine upon exit
;=============================================================================
; Preserves X register
;*****************************************************************************
.proc	xv_setisr: near
	nop

	lda	#OP_NOP		; NOP
	sta	xv_clearisr	; Ungate xv_clearisr
	lda	#OP_RTS		; RTS
	sta	xv_setisr	; Gate xv_setisr

	; Save ZP ptr to stack
	SAVE_PTR X16_PTR_0

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
	RESTORE_PTR X16_PTR_0

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

;*****************************************************************************
; Disable the ISR routine by reinstalling the original IRQ vector
;=============================================================================
; Preserves X register
;*****************************************************************************
.proc	xv_clearisr: near
	rts	; Gate

	lda	#OP_NOP		; NOP
	sta	xv_setisr	; Ungate xv_setisr
	lda	#OP_RTS		; RTS
	sta	xv_clearisr	; Gate xv_clearisr

	; Save ZP ptr to stack
	SAVE_PTR X16_PTR_0

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
	RESTORE_PTR X16_PTR_0
	rts
.endproc

;*****************************************************************************
; Zeroes out variable space, copies ISR to lowram and ensures ISR loads 
; correct RAM bank before calling xv_tick to actually handle the interrupt.
;=============================================================================
; Arguments: A and X are low- and high-byte of lowram address reserved for
;            the x16vision library
;-----------------------------------------------------------------------------
; Preserves: X and Y
; Returns: (none)
;*****************************************************************************
.proc	xv_initialize: near
	phy	; Preserve Y
	tay	; Save low byte of lowram address in Y
	; Save ZP ptr to stack
	SAVE_PTR X16_PTR_0
	phy	; Save low byte of lowram on stack

	; Set ZP ptr to address of variables
	lda	#<__XVKITBSS_LOAD__
	sta	X16_PTR_0
	lda	#>__XVKITBSS_LOAD__
	sta	X16_PTR_0+1
	; Zero out variables
	lda	#0
	ldy	#<(__XVKITBSS_SIZE__-1)
@zloop:
	sta	(X16_PTR_0),y
	dey
	bpl	@zloop

	; Calculate the remaining free space in the current bank and store
	; the value in the rem_space variable - this is the amount of
	; space the user has for creating objects
	lda	#<(X16_RAM_Window+X16_RAM_WindowSize-START_OF_LINKED_LIST)
	sta	rem_space
	lda	#>(X16_RAM_Window+X16_RAM_WindowSize-START_OF_LINKED_LIST)
	sta	rem_space+1

	pla	; Restore low byte of lowram from stack
	; Save low byte of lowram address and write it to ZP ptr
	sta	lowram_addr
	sta	X16_PTR_0
	; Save high byte of lowram address and write it to ZP ptr
	stx	lowram_addr+1
	stx	X16_PTR_0+1
	; Save current RAM bank, this is where x16vision library is loaded
	lda	X16_RAMBank_Reg
	sta	ISRBANK

	; Copy routines to low ram
	ldy	#(_end_xvkit_lowram-_isr-1) ; Size of xvkit_lowram segment
@loop:
	lda	_isr,y
	sta	(X16_PTR_0),y
	dey
	bpl	@loop

	; Restore ZP ptr
	RESTORE_PTR X16_PTR_0

	; Calculate addresses of helper functions in lowram and store them
	; in the jumptable
	lda	#(_lda_bank-_isr)
	clc
	adc	lowram_addr
	sta	lda_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	lda_bank+2

	lda	#(_ldxa_bank-_isr)
	clc
	adc	lowram_addr
	sta	ldxa_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	ldxa_bank+2

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

	ply	; Restore Y
	rts
.endproc

.segment "XVKIT_LOWRAM"
;*****************************************************************************
; Ensures the interrupt was generated by VSYNC, otherwise it just calls the
; previous interrupt handler.
; If VSYNC generated the interrupt, the current RAM bank is saved, the RAM
; bank where x16vision library is installed is set and a jsr to xv_tick
; function is performed before continuing on to previous interrupt handler
;=============================================================================
; ROM has already preserved registers and previous interrupt handler should
; ensure they are restored. RAM bank is restored before call to original
; interrupt handler
;*****************************************************************************
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

;*****************************************************************************
; Load a byte from banked RAM pointed to by ZP pointer X16_PTR_0 in bank
; specified by the value in .X
;=============================================================================
; Arguments: X16_PTR_0, ZP pointer to the memory address that should be read
;            .X holds the RAM bank to read from
;-----------------------------------------------------------------------------
; Preserves: .X and .Y and the RAM bank before call
; Returns: .A contains the value read from memory
;*****************************************************************************
_lda_bank:
	phy			; Preserve .Y
	ldy	X16_RAMBank_Reg	; Save current RAM bank
	stx	X16_RAMBank_Reg	; Set new RAM bank
	lda	(X16_PTR_0)	; Load value from address pointed to by ZP pointer
	sty	X16_RAMBank_Reg	; Restore RAM bank
	ply			; Restore .Y
	rts

;*****************************************************************************
; Load two bytes from banked RAM pointed to by ZP pointer X16_PTR_0 in bank
; specified by the value in .X
;=============================================================================
; Arguments: X16_PTR_0, ZP pointer to the memory address that should be read
;            .X holds the RAM bank to read from
;-----------------------------------------------------------------------------
; Preserves: .Y and the RAM bank before call
; Returns: .X contains value from ZP pointer, .A from ZP pointer + 1
;*****************************************************************************
_ldxa_bank:
	phy			; Preserve .Y
	ldy	X16_RAMBank_Reg	; Save current RAM bank to stack
	phy
	ldy	#1		; Initialize .Y for reading value through ZP pointer
	stx	X16_RAMBank_Reg	; Set new RAM bank
	lda	(X16_PTR_0)	; Load value from address pointed to by ZP pointer
	tax			; Store the value in .X
	lda	(X16_PTR_0),y	; Load next value from address pointed to by ZP pointer with .Y added
	ply			; Restore RAM bank from stack
	sty	X16_RAMBank_Reg
	ply			; Restore .Y
	rts

;*****************************************************************************
; Load three bytes from banked RAM pointed to by ZP pointer X16_PTR_0 in bank
; specified by the value in .X
;=============================================================================
; Arguments: X16_PTR_0, ZP pointer to the memory address that should be read
;            .X holds the RAM bank to read from
;-----------------------------------------------------------------------------
; Preserves: The RAM bank before call
; Returns: .Y=ZP pointer, .X=ZP pointer+1, .A=ZP pointer+2
;*****************************************************************************
_ldyxa_bank:
	ldy	X16_RAMBank_Reg	; Save current RAM bank
	stx	X16_RAMBank_Reg	; Set new RAM bank
	lda	(X16_PTR_0)	; Load value from address pointed to by ZP pointer
	pha			; Save value to stack
	phy			; Save RAM bank to stack
	ldy	#1		; Initialize .Y for reading value through ZP pointer
	lda	(X16_PTR_0),y	; Load next value from address pointed to by ZP pointer with .Y added
	tax			; Store value in .X
	iny
	lda	(X16_PTR_0),y	; Load next value from address pointed to by ZP pointer with .Y added
	ply			; Restore original RAM bank
	sty	X16_RAMBank_Reg
	ply			; Get first read value from stack
	rts

;*****************************************************************************
; Store a byte to banked RAM pointed to by ZP pointer X16_PTR_0 in bank
; specified by the value in .X
;=============================================================================
; Arguments: X16_PTR_0, ZP pointer to the memory address that should be written
;            .X holds the RAM bank to write to
;	     .A holds the value to write
;-----------------------------------------------------------------------------
; Preserves: .A, .X, .Y and the RAM bank before call
; Returns: nothing
;*****************************************************************************
_sta_bank:
	phy			; Preserve .Y on stack
	ldy	X16_RAMBank_Reg	; Save current RAM bank
	stx	X16_RAMBank_Reg	; Set new RAM bank
	sta	(X16_PTR_0)	; Store value in .A to address pointed to by ZP pointer
	sty	X16_RAMBank_Reg	; Restore RAM bank
	ply			; Restore .Y from stack
	rts

;*****************************************************************************
; Store two bytes to banked RAM pointed to by ZP pointer X16_PTR_0 in bank
; specified by the value in .X
;=============================================================================
; Arguments: X16_PTR_0, ZP pointer to the memory address that should be written
;            .X holds the RAM bank to write to
;	     .A & .Y holds the values to write in that order (low-high)
;-----------------------------------------------------------------------------
; Preserves: The RAM bank before call
; Returns: nothing
;*****************************************************************************
_stay_bank:
	pha			; Preserve value in .A
	lda	X16_RAMBank_Reg	; Save current RAM bank in .A
	stx	X16_RAMBank_Reg	; Set the new RAM bank
	plx			; Pull .A value from stack and store it in .X temporarily
	pha			; Push original RAM bank to stack
	txa			; Move .A value from .X back to .A
	sta	(X16_PTR_0)	; Store value in .A to address pointed to by ZP pointer
	tya			; Store value in .Y to address pointed to by ZP pointer with .Y added
	ldy	#1
	sta	(X16_PTR_0),y
	pla			; Restore RAM bank
	sta	X16_RAMBank_Reg
	rts

_end_xvkit_lowram:

.assert __XVKIT_LOWRAM_SIZE__ <= 255, error, "XVKIT_LOWRAM segment may not be larger than 255 bytes"
.assert __XVKITBSS_SIZE__ <= 255, error, "XVKITBSS segment is larger than 255 bytes, check zeroing code"