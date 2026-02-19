.include "x16.inc"
.include "macros.inc"
.include "x16vision.inc"
.include "memman.inc"

.import __XVKITVARS_SIZE__, __XVKITVARS_LOAD__

; Imports from x16internals.s
.import desktop_handle, jiffiecnt, xv_tick
.import scr_width, scr_height, petcp

; Imports from vtui.s
.import vtui_setbank, vtui_setstride, vtui_setdecr, vtui_clrscr
.import vtui_gotoxy, vtui_plotchar, vtui_scanchar, vtui_hline
.import vtui_vline, vtui_fillbox, vtui_saverect, vtui_restrect
.import vtui_getbank, vtui_getstride, vtui_getdecr
.import vtui_width, vtui_height

X16VISION_VERSION	= $0001

.segment "HEADER"
; Variables holding the "header" of the mmb
free_addr:	.res 2
first_item:	.res 2
id_bitmap:	.res 32

.segment "JUMPTABLE"
	jmp	_xv_initialize	; $A024
	jmp	mm_set_isr	; $A027
	jmp	mm_clear_isr	; $A02A
	jmp	_xv_desktop	; $A02D
	jmp	_xv_statusbar	; $A030
	jmp	_xv_menubar	; $A033

.segment "XVKITLIB"

;*****************************************************************************
; Create a menu bar and set the values for it
;=============================================================================
; Inputs:	.A = Color
;		.Y = Selected/Highlight color
; Outputs:	.C set on error with errorcode in .A
;-----------------------------------------------------------------------------
; Preserves:	Nothing
;*****************************************************************************
.proc	_xv_menubar: near
	pha
	phy
	DESKTOP_PTR 2
	; Desktop is now 1 line shorter because of the status bar
	ldy	#XV_DESKTOP_HEIGHT
	jsr	mm_lda_bank
	dec
	jsr	mm_sta_bank
	; Desktop Y start is incremented by 1 line
	iny	; XV_DESKTOP_Y_START
	jsr	mm_lda_bank
	inc
	jsr	mm_sta_bank
	; Update the select/highlight color
	ldy	#XV_MENU_BAR_SEL_COLOR
	pla
	jsr	mm_sta_bank
	; Update the normal color
	dey	;XV_MENU_BAR_COLOR
	pla
	jsr	mm_sta_bank
	; Update the dirty bit
	dey	;XV_MENU_BAR_DIRTY
	lda	#1
	jsr	mm_sta_bank
	clc
	rts
.endproc

;*****************************************************************************
; Create a status bar and set the values for it
;=============================================================================
; Inputs:	.A = Color
;		.Y = Selected/Highlight color
; Outputs:	.C set on error with errorcode in .A
;-----------------------------------------------------------------------------
; Preserves:	Nothing
;*****************************************************************************
.proc	_xv_statusbar: near
	pha
	phy
	DESKTOP_PTR 2
	; Desktop is now 1 line shorter because of the status bar
	ldy	#XV_DESKTOP_HEIGHT
	jsr	mm_lda_bank
	dec
	jsr	mm_sta_bank
	; Update the select/highlight color
	ldy	#XV_STATUS_SEL_COLOR
	pla
	jsr	mm_sta_bank
	; Update the normal color
	dey	;XV_STATUS_COLOR
	pla
	jsr	mm_sta_bank
	; Update the dirty bit
	dey	;XV_STATUS_DIRTY
	lda	#1
	jsr	mm_sta_bank
	clc
	rts
.endproc

;*****************************************************************************
; Allocate memory for the desktop structure, save information about the
; desktop and mark it as dirty to get it drawn by the interrupt handler
;=============================================================================
; Inputs:	.A = Background Character
;		.X = Background Color
;		.Y = Screen Mode
; Outputs:	.C set on error with errorcode in .A
;-----------------------------------------------------------------------------
; Preserves:	Nothing
;*****************************************************************************
.proc	_xv_desktop: near
	pha	; Character
	phx	; Color
	phy	; Screen Mode
	; Allocate memory for the desktop structure
	ldx	desktop_handle+0	; Get RAM bank
	lda	#<XV_DESKTOP_STRUCT_SIZE
	ldy	#>XV_DESKTOP_STRUCT_SIZE
	jsr	mm_alloc
	bcs	error
	; Save the handle
	sta	desktop_handle+0
	sty	desktop_handle+1
	jsr	mm_get_ptr	; Get pointer to desktop structure
	bcs	error
	; Save the pointer
	jsr	mm_store_zp1	; Set ZP pointer
	; Zero allocated memory
	ldy	#XV_DESKTOP_STRUCT_SIZE-1
	lda	#0
zloop:	jsr	mm_sta_bank
	dey
	bne	zloop
	jsr	mm_sta_bank
	; Set the chosen screen mode
	pla	; Screen Mode
	pha
	clc
	jsr	X16_Kernal_screen_mode
	lda	#XV_ERR_MODE_UNSUPPORTED
	bcs	error
	; Set .X to memory bank after call to screen_mode
	ldx	desktop_handle+0	; RAM Bank
	; Write the screen height to the desktop structure
	ply				; Screen Mode
	lda	scr_height,y
	phy				; Screen Mode
	ldy	#XV_DESKTOP_HEIGHT
	jsr	mm_sta_bank
	; Write the screen width to the desktop structure
	ply				; Screen Mode
	lda	scr_width,y
	ldy	#XV_DESKTOP_WIDTH
	jsr	mm_sta_bank
	; Write the color information to the desktop structure
	ldy	#XV_DESKTOP_COL
	pla				; Color
	jsr	mm_sta_bank
	; Write the character information to the desktop structure
	ldy	#XV_DESKTOP_CHAR
	pla				; Character
	jsr	mm_sta_bank
	; Tell the library that the desktop needs to be redrawn
	ldy	#XV_DESKTOP_DIRTY
	lda	#1
	jsr	mm_sta_bank
	clc
	rts
error:	ply	; Clear stack
	ply
	ply
	rts
.endproc

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
	lda	#1
	sta	jiffiecnt
	stx	desktop_handle+0	; Store allocated Bank for later use
	; Ensure that we get VSYNC interrupts from VERA
	lda	Vera_Reg_IEN
	ora	#$01
	sta	Vera_Reg_IEN
	; Set the xv_tick interrupt handler
	lda	#<xv_tick
	ldy	#>xv_tick
	ldx	X16_RAMBank_Reg
	jsr	mm_set_isr
	; Check character set in VERA for supported character sets
	lda	#$11
	sta	Vera_Reg_AddrH
	lda	#>$F382
	sta	Vera_Reg_AddrM
	lda	#<$F382
	sta	Vera_Reg_AddrL
	lda	Vera_Reg_Data0
	cmp	#$DC		; Only CP437 charset have $DC at address $1F382
	bne	:+
	lda	#7		; Using CP437 charset (#7)
	sta	petcp
	; handle CP437 charset
	clc
	rts
:	cmp	#$00		; Only PET charsets have $00 at address $1F382
	bne	unsupported
	; handle PET charsets
	lda	#>$F280
	sta	Vera_Reg_AddrM
	lda	#<$F280
	sta	Vera_Reg_AddrL
	lda	Vera_Reg_Data0
	cmp	#$FF		; PET Uppercase / Graphics have $FF at $1F280
	bne	pet_uplo
	lda	#2		; Using PET-GFX charset (#2 or #4)
	sta	petcp
	; Handle PET Upper/Gfx charsets
	clc
	rts
pet_uplo:
	; Handle PET Upper/lower charsets
	lda	#3		; Using PET-Up/Lo charset (#3 or #5)
	sta	petcp
	clc
	rts
unsupported:
	lda	#$FF
	sta	petcp
	lda	#XV_ERR_CHARSET_UNSUPPORTED
	sec
	rts
.endproc
;01: 1F380: 00 00 6C 76 66 7C 60 60	1F280:
;02: 1F380: 00 00 00 1F 1F 18 18 18	1F280: FF FF 03 03 03 03 03 03
;03: 1F380: 00 00 00 1F 1F 18 18 18	1F280: 7C 66 66 7C 60 60 60 00
;04: 1F380: 00 00 00 00 0F 08 08 08	1F280: FF 01 01 01 01 01 01 01
;05: 1F380: 00 00 00 00 0F 08 08 08	1F280: 7C 42 42 7C 40 40 40 00
;06: 1F380: 00 00 5C 62 62 5C 40 40	1F280:
;07: 1F380: 00 00 DC 66 66 7C 60 F0	1F280:
;08: 1F380: 00 00 6C 76 66 7C 60 60	1F280:
;09: 1F380: 00 00 5C 62 62 5C 40 40	1F280:
;10: 1F380: 00 00 6C 76 66 7C 60 60	1F280:
;11: 1F380: 00 00 5C 62 62 5C 40 40	1F280:
;12: 1F380: 00 00 5c 62 62 5c 40 40	1F280:

.assert __XVKITVARS_SIZE__ <= 255, error, "XVKITVARS segment is larger than 255 bytes, check zeroing code"
