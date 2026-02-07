.include "x16.inc"
.include "macros.inc"
.include "x16vision.inc"
.include "memman.inc"

.import __XVKITVARS_SIZE__, __XVKITVARS_LOAD__

; Imports from vtui.s
.import vtui_initialize, vtui_screenset, vtui_setbank, vtui_setstride
.import vtui_setdecr, vtui_clrscr, vtui_gotoxy, vtui_plotchar
.import vtui_scanchar, vtui_hline, vtui_vline, vtui_printstr
.import vtui_fillbox, vtui_pet2scr, vtui_scr2pet, vtui_border
.import vtui_saverect, vtui_restrect, vtui_inputstr, vtui_getbank
.import vtui_getstride, vtui_getdecr

X16VISION_VERSION	= $0001

.segment "HEADER"
; Variable holding the next free address in the bank
free_addr:	.res 2
first_item:	.res 2
id_bitmap:	.res 32

.segment "JUMPTABLE"
	jmp	_xv_initialize	; $A024
	jmp	mm_set_isr	; $A027
	jmp	mm_clear_isr	; $A02A
	jmp	_xv_desktop	; $A02D
	jmp	_xv_statusbar	; $A030

.segment "XVKITVARS"
desktop_addr:	.res 2
desktop_handle:	.res 2

.segment "XVKITLIB"
scr_width:	.byte 80,80,40,40,40,20,20,22,64,64,32,32
scr_height:	.byte 60,30,60,30,15,30,15,23,50,25,50,25

;*****************************************************************************
; Initializes memory manager, zeroes out variable space
;=============================================================================
; Inputs:	.A = First ZP address to use as pointer
;		.Y = Second ZP address to use as pointer
;		.X = First RAM bank to allocate memory in
;		Content of first ZP pointer should be the lowram address
;		Content of second ZP pointer should be first free address
;		  in RAM bank. $A000 if it is an empty RAM bank.
;		  Any RAM bank used by the library must reserve the first 36
;		  bytes of the RAM Bank for the library.
;-----------------------------------------------------------------------------
; Preserves:	.X
;*****************************************************************************
.proc	_xv_initialize: near
	jsr	mm_init
	lda	#<__XVKITVARS_LOAD__
	ldy	#>__XVKITVARS_LOAD__
	jsr	mm_store_zp1
	; Zero out variables
	lda	#0
	ldy	#<(__XVKITVARS_SIZE__-1)
zeroloop:
	jsr	mm_sta_bank
	dey
	bne	zeroloop
	jsr	mm_sta_bank
	stx	desktop_handle+0
	rts
.endproc

;*****************************************************************************
;=============================================================================
; Inputs:	.A = Background Character
;		.X = Background Color
;		.Y = Screen Mode
;-----------------------------------------------------------------------------
;*****************************************************************************
.proc	_xv_desktop: near
	pha	; Character
	phx	; Color
	phy	; Screen Mode
	ldx	desktop_handle+0
	lda	#<XV_DESKTOP_STRUCT_SIZE
	ldy	#>XV_DESKTOP_STRUCT_SIZE
	jsr	mm_alloc
	bcs	error
	sta	desktop_handle+0
	sty	desktop_handle+1
	jsr	mm_get_ptr
	bcs	error
	sta	desktop_addr+0
	sty	desktop_addr+1
	jsr	mm_store_zp1
	pla	; Screen Mode
	pha
	clc
	jsr	X16_Kernal_screen_mode
	lda	#XV_ERR_MODE_UNSUPPORTED
	bcs	error
	ldx	desktop_handle+0
	ply	; Screen Mode
	lda	scr_height,y
	phy
	ldy	#XV_DESKTOP_HEIGHT
	jsr	mm_sta_bank
	ply	; Screen Mode
	lda	scr_width,y
	ldy	#XV_DESKTOP_WIDTH
	jsr	mm_sta_bank
	ldy	#XV_DESKTOP_COL
	pla	; Color
	jsr	mm_sta_bank
	ldy	#XV_DESKTOP_CHAR
	pla	; Character
	pha
	jsr	mm_sta_bank
	ldy	#XV_DESKTOP_COL
	jsr	mm_lda_bank
	tax
	pla	; Character
	jmp	vtui_clrscr

error:	ply	; Clear stack
	ply
	ply
	rts
.endproc

;*****************************************************************************
;=============================================================================
;-----------------------------------------------------------------------------
;*****************************************************************************
.proc	_xv_statusbar: near
	rts
.endproc

;*****************************************************************************
;=============================================================================
;-----------------------------------------------------------------------------
;*****************************************************************************
.proc	xv_tick: near
	rts
.endproc

.assert __XVKITVARS_SIZE__ <= 255, error, "XVKITVARS segment is larger than 255 bytes, check zeroing code"