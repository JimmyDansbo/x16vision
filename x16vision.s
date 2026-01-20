.include "x16.inc"
.include "macros.inc"
.include "x16vision.inc"
.include "memman.inc"

.import __XVKITBSS_SIZE__, __XVKITBSS_LOAD__

; Imports from vtui.s
.import vtui_initialize, vtui_screenset, vtui_setbank, vtui_setstride
.import vtui_setdecr, vtui_clrscr, vtui_gotoxy, vtui_plotchar
.import vtui_scanchar, vtui_hline, vtui_vline, vtui_printstr
.import vtui_fillbox, vtui_pet2scr, vtui_scr2pet, vtui_border
.import vtui_saverect, vtui_restrect, vtui_inputstr, vtui_getbank
.import vtui_getstride, vtui_getdecr

X16VISION_VERSION	= $0001
MINIMUM_ROMVERSION	= 48

.segment "HEADER"
; Variable holding amount of free space in the bank
rem_space:	.word 0
free_addr:	.word 0
.segment "JUMPTABLE"
XVINIT:	; Label to let me know in the symbols file where jumptable starts
	jmp	_xv_initialize	; $A004
;	jmp	_xv_setisr	; $A007
;	jmp	_xv_clearisr	; $A00A
	jmp	_xv_desktop	; $A00D
	jmp	_xv_statusbar	; $A010

.segment "XVKITBSS"

; Lowram address provided by the user to copy routines to.
lowram_addr:	.res 2
; Object handled by library are in a linked list, this points to the start of the list.
init_obj_addr:	.res 2
init_obj_bank:	.res 1
; This points to the latest object for ease of adding objects.
last_obj_addr:	.res 2
last_obj_bank:	.res 1
; Space reserved for saving ZP pointers before using them
preserve_ptr:	.res 4
; Space reserved for the desktop information
desktop:	.res XV_DESKTOP_STRUCT_SIZE

START_OF_LINKED_LIST:

.segment "XVKITLIB"

OP_BRA=$80	; Opcode for BRA
OP_LDA_IMM=$A9	; Opcode for LDA #

;*****************************************************************************
;=============================================================================
;*****************************************************************************
.proc	xv_tick: near

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
;
;=============================================================================
; Inputs:	placement = top or bottom
;		text alignment right, left, center
;		color
;		highlight color
;		string length
;		pointer to string
;		bank of string
;*****************************************************************************
.proc	_xv_statusbar: near
	GATE_THIS_FUNCTION
	STACK_SAVE_PTR X16_PTR_0

	lda	#2
	jsr	vtui_setstride
	lda	#0
	ldy	#0
	jsr	vtui_gotoxy

;	lda	#$A0
;	sta	X16_PTR_0+1
;	stz	X16_PTR_0
	ldy	#12
	ldx	#11
	jsr	printstr

	lda	#1
	jsr	vtui_setstride

	STACK_RESTORE_PTR X16_PTR_0
	rts
.endproc

;*****************************************************************************
; Print a string. If pointer to string is $A000 or higher, RAM bank must be
; specified.
; Function expects stride to be set to 2 so no color information needed
;=============================================================================
; Inputs:	X16_PTR_0 ZP pointer to string
;		Y length of string
;		X bank, if in banked memory
; Uses:		A
;*****************************************************************************
.proc	printstr: near
	lda	X16_Reg_X0L
	pha
	sty	X16_Reg_X0L
	ldy	#0
@loop:	cpy	X16_Reg_X0L
	beq	@end
	lda	X16_PTR_0+1
	cmp	#$A0
	bcs	@banked
	lda	(X16_PTR_0),y
	bra	:+
@banked:
;	jsr	lda_bank
:	sta	Vera_Reg_Data0
	iny
	bra	@loop
@end:	pla
	sta	X16_Reg_X0L
	rts
.endproc

;*****************************************************************************
; Create, draw and save information about the desktop element which must be
; the first element created by the library
;=============================================================================
; Input: Screen Mode	= R0L
;	 Character Set	= R0H
;	 Output mode	= R1L
;	 Background char= A
;	 Color		= X
;*****************************************************************************
.proc	_xv_desktop: near
	GATE_THIS_FUNCTION
	sta	desktop+XV_DESKTOP_CHAR
	stx	desktop+XV_DESKTOP_COL
	
	lda	X16_Reg_R0L	; Set Screen Mode
	clc
	jsr	X16_Kernal_screen_mode
	sec			; Get height and width of current screen mode
	jsr	X16_Kernal_screen_mode
	sty	desktop+XV_DESKTOP_HEIGHT
	stx	desktop+XV_DESKTOP_WIDTH

	lda	X16_Reg_R0H	; Load character set
	jsr	X16_Kernal_screen_set_charset
	lda	X16_Reg_R1L	; Load output mode
	and	#$0F		; Ensure only low nibble is set
	ora	Vera_Reg_DCVideo
	sta	Vera_Reg_DCVideo

	lda	#1
	jsr	vtui_setstride
	clc
	jsr	vtui_setdecr
	lda	desktop+XV_DESKTOP_CHAR
	ldx	desktop+XV_DESKTOP_COL
	jsr	vtui_clrscr
	rts
.endproc


;*****************************************************************************
; Reads out the ROM version from address $FF80 in ROM bank 0
;=============================================================================
; Preserves: X and Y
; Returns: current ROM version in A
;*****************************************************************************
.proc	getromver: near
	lda	X16_ROMBank_Reg	; If we are already in ROM Bank 0
	bne	:+
	lda	$FF80		; Read ROM version and return
	rts
:	phy			; Else preserve Y and switch ROM bank
	ldy	X16_ROMBank_Reg
	stz	X16_ROMBank_Reg
	lda	$FF80
	sty	X16_ROMBank_Reg
	ply			; Restore Y
	pha	; Push and Pull A to ensure N flag is correct
	pla
	rts
.endproc

;*****************************************************************************
; Zeroes out variable space, copies functions to lowram and ensures ISR loads 
; correct RAM bank before calling xv_tick to actually handle the interrupt.
;=============================================================================
; Arguments: A and X are low- and high-byte of lowram address reserved for
;            the x16vision library
;-----------------------------------------------------------------------------
; Preserves: X and Y
; Returns: Carry set on error, A = error code
;*****************************************************************************
.proc	_xv_initialize: near
	jsr	mm_init_lowram

	; Calculate the remaining free space in the current bank and store
	; the value in the rem_space variable - this is the amount of
	; space the user has for creating objects
	lda	#<(X16_RAM_Window+X16_RAM_WindowSize-START_OF_LINKED_LIST)
	sta	rem_space
	lda	#>(X16_RAM_Window+X16_RAM_WindowSize-START_OF_LINKED_LIST)
	sta	rem_space+1

	lda	#<START_OF_LINKED_LIST
	sta	free_addr
	lda	#>START_OF_LINKED_LIST
	sta	free_addr+1
	ldx	#$10

	lda	#200
	jsr	mm_alloc
	jsr	mm_remaining

	rts
	lda	#<xv_tick
	ldx	#>xv_tick
	ldy	#$10 ; this is the same as in test.asm
	sty	$30
	ldy	#$30
	jsr	mm_set_isr
	rts





	sta	lowram_addr	; Save low byte of lowram address to mem
	; Ensure that we are are on a supported ROM
	jsr	getromver
	bmi	@isprerelease
	sta	lowram_addr+1	; Save ROM version temporarily
	lda	#XV_NOERR	; Push NOERR onto stack
	pha
	lda	lowram_addr+1	; Restore ROM version to A
	cmp	#MINIMUM_ROMVERSION
	bcs	@continue
	pla			; clear stack
	lda	#XV_WRONGROM
	sec
	rts
@isprerelease:
	sta	lowram_addr+1	; Save ROM version temporarily
	lda	#XV_PRERELROM	; Push Pre Release ROM warning onto stack
	pha
	lda	lowram_addr+1	; Restore ROM version to A
	eor	#$FF
	cmp	#MINIMUM_ROMVERSION
	bcs	@continue
	pla			; clear stack
	lda	#XV_WRONGROM
	sec
	rts
@continue:
	phy	; Preserve Y
	lda	lowram_addr	; Preserve low byte of lowram address on stack
	pha
	; Save ZP ptr
	SAVE_PTR X16_PTR_0

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
	bne	@zloop
	sta	(X16_PTR_0)


	; Write low byte of lowram address to ZP ptr
	pla
	sta	lowram_addr
	sta	X16_PTR_0
	; Save high byte of lowram address and write it to ZP ptr
	stx	lowram_addr+1
	stx	X16_PTR_0+1
	; Save current RAM bank, this is where x16vision library is loaded
	lda	X16_RAMBank_Reg
;	sta	ISRBANK

;	jsr	vtui_initialize

	ply	; Restore Y
	pla	; Get err/warn code from stack
	clc	; Clear carry to to show initialization successfull
	rts
.endproc



.assert __XVKITBSS_SIZE__ <= 255, error, "XVKITBSS segment is larger than 255 bytes, check zeroing code"