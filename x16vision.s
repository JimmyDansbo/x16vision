.include "x16.inc"
.include "macros.inc"
.include "x16vision.inc"
.include "memman.inc"

.macro TEST
.charmap $61, $01
.endmacro

.import __XVKITVARS_SIZE__, __XVKITVARS_LOAD__

; Imports from vtui.s
.import vtui_initialize, vtui_screenset, vtui_setbank, vtui_setstride
.import vtui_setdecr, vtui_clrscr, vtui_gotoxy, vtui_plotchar
.import vtui_scanchar, vtui_hline, vtui_vline, vtui_printstr
.import vtui_fillbox, vtui_pet2scr, vtui_scr2pet, vtui_border
.import vtui_saverect, vtui_restrect, vtui_inputstr, vtui_getbank
.import vtui_getstride, vtui_getdecr
.import vtui_width, vtui_height

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
	jmp	_xv_menubar	; $A033

.segment "XVKITVARS"
desktop_handle:	.res 2
petcp:		.res 1
jiffiecnt:	.res 1
year:		.res 1
month:		.res 1
day:		.res 1
weekday:	.res 1
hour:		.res 1
minute:		.res 1
second:		.res 1

.segment "XVKITLIB"
scr_width:	.byte 80,80,40,40,40,20,20,22,64,64,32,32
scr_height:	.byte 60,30,60,30,15,30,15,23,50,25,50,25

; TR=TopRight, TL=TopLeft, BR=BottomRight, BL=BottomLeft, HO=Horizontal, VE=Vertical
; TT=TopT, BT=BottomT, LT=LeftT, RT=RightT, CR=Cross
;		      TR   TL   BR   BL   HO   VE   TT	 BT   LT   RT	CR
cp437_charset:	.byte $AA, $A9, $D9, $C0, $C4, $B3, $C2, $C1, $C3, $B4, $C5
pet_charset:	.byte $6E, $70, $7D, $6D, $40, $42, $72, $71, $6B, $73, $5B

;*****************************************************************************
;=============================================================================
;-----------------------------------------------------------------------------
;*****************************************************************************
.proc _xv_menubar: near

	lda	#80
	ldy	#$B0+30
	sta	$9F20
	sty	$9F21
	ldx	#$10
	DBG
	lda	#'@'
	jsr	convert
	sta	$9F23
	stx	$9F23
	lda	#']'
	jsr	convert
	sta	$9F23
	stx	$9F23
	lda	#'!'
	jsr	convert
	sta	$9F23
	stx	$9F23
	lda	#'?'
	jsr	convert
	sta	$9F23
	stx	$9F23
	lda	#'A'
	jsr	convert
	sta	$9F23
	stx	$9F23
	lda	#'a'
	jsr	convert
	sta	$9F23
	stx	$9F23
	lda	#'0'
	jsr	convert
	sta	$9F23
	stx	$9F23


	rts
.endproc

;*****************************************************************************
; Convert ASCII character to screen code according to the selected
; character set
;=============================================================================
; Input:	.A = Character to convert if necessary
; Output:	.A = Converted character
;		.C set if conversion was performed
;-----------------------------------------------------------------------------
; Depends:	Expects petcp variable to be set correctly
; Preserves:	All except .A
;*****************************************************************************
.proc convert: near
	phy		; Save .Y
	ldy	petcp	; Check character set
	cpy	#2
	bne	uplo	; If not PET - UP/GFX, continue to PET - UP/LO
	; PET - UP/GFX
	cmp	#$80
	bcc	noconv	; If >= $80, no conversion
	cmp	#$40
	bcc	noconv	; If < $40, no conversion
	and	#$9F	; Reset bits 5 & 6
	bra	conv
uplo:	; PET - UP/LO
	cpy	#3
	bne	noconv	; If not PET - UP/LO, no conversion will be done
	cmp	#$80
	bcs	noconv	; If >= $80 no conversion
	cmp	#$60
	bcc	:+	; If >= $60, convert
	and	#$9F	; Reset bits 5 & 6
	bra	conv
:	cmp	#$40	; $40 is a special case
	bne	noconv	; If not $40, no convert
	lda	#$00	; If $40, convert to $00
conv:	ply
	sec
	rts
noconv:	ply
	clc
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
	lda	#60
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
	lda	#3		; Using PET-Up/Lo charset (#3 or #5)
	sta	petcp
	; Handle PET Upper/lower charsets
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
;	sta	desktop_addr+0
;	sty	desktop_addr+1
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
	rts
error:	ply	; Clear stack
	ply
	ply
	rts
.endproc

;*****************************************************************************
;=============================================================================
;-----------------------------------------------------------------------------
;*****************************************************************************
.proc update_time: near
	ldx	#X16_I2C_RTC
	ldy	#X16_RTCReg_ClockSeconds
	jsr	X16_Kernal_i2c_read_byte
	and	#$7F
retry:	sta	second
	iny	; 1 minutes
	jsr	X16_Kernal_i2c_read_byte
	cmp	minute
	bne	:+
	rts
:	sta	minute
	iny	; 2 hours
	jsr	X16_Kernal_i2c_read_byte
	cmp	hour
	bne	:+
	rts
:	sta	hour
	iny	; 3 weekday
	jsr	X16_Kernal_i2c_read_byte
	sta	weekday
	iny	; 4 day of month
	jsr	X16_Kernal_i2c_read_byte
	sta	day
	iny	; 5 month
	jsr	X16_Kernal_i2c_read_byte
	sta	month
	iny	; 6 year
	jsr	X16_Kernal_i2c_read_byte
	sta	year
	ldy	#X16_RTCReg_ClockSeconds
	jsr	X16_Kernal_i2c_read_byte
	and	#$7F
	cmp	second
	bne	retry
	rts
.endproc

;*****************************************************************************
;=============================================================================
; Output:	.C clear if update work has been done
;-----------------------------------------------------------------------------
;*****************************************************************************
.proc update_desktop: near
	; Set memory bank and ZP pointer
	lda	desktop_handle+0	; If desktop handle has not yet been set, quit
	bne	:+
	lda	#XV_ERR_NO_DESKTOP
	sec
	rts
:	ldy	desktop_handle+1
	jsr	mm_get_ptr
	jsr	mm_store_zp1
	; Check if desktop needs to be redrawn
	ldy	#XV_DESKTOP_DIRTY
	jsr	mm_lda_bank
	bne	:+
	sec
	rts
	; Read Y start value  and go to correct starting coordinates
:	ldy	#XV_DESKTOP_Y_START
	jsr	mm_lda_bank
	tay
	lda	#0
	jsr	vtui_gotoxy
	; Read width and height and store in ZP registers
	ldy	#XV_DESKTOP_WIDTH
	jsr	mm_lday_bank
	sta	vtui_width
	sty	vtui_height
	; Read character and color and store in correct registers
	ldy	#XV_DESKTOP_CHAR
	jsr	mm_lday_bank
	phy			; Transfer color from .Y to .X through stack
	plx
	jsr	vtui_fillbox
	; Update the desktop dirty bit
	ldx	desktop_handle+0
	ldy	#XV_DESKTOP_DIRTY
	lda	#0
	jsr	mm_sta_bank
	clc
	rts
.endproc

;*****************************************************************************
; Create a status bar and set the values for it
;=============================================================================
; Inputs:	.A = Color
;		.Y = Selected/Highlight color
;-----------------------------------------------------------------------------
;*****************************************************************************
.proc	_xv_statusbar: near
	pha
	phy
	lda	desktop_handle+0
	bne	:+
	ply		; Clear stack
	ply
	lda	#XV_ERR_NO_DESKTOP
	sec
	rts
:	ldy	desktop_handle+1
	jsr	mm_get_ptr
	bcc	:+
	ply		; Clear stack
	ply
	rts
:	jsr	mm_store_zp1
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
	rts
.endproc

;*****************************************************************************
;=============================================================================
; Output:	.C clear if update work has been done
;-----------------------------------------------------------------------------
;*****************************************************************************
.proc update_statusbar: near
	lda	desktop_handle+0
	bne	:+
	lda	#XV_ERR_NO_DESKTOP
	sec
	rts
:	ldy	desktop_handle+1
	jsr	mm_get_ptr
	bcc	:+
	rts
:	jsr	mm_store_zp1
	ldy	#XV_STATUS_DIRTY
	jsr	mm_lda_bank
	bne	:+
	sec
	rts
:	ldy	#XV_DESKTOP_HEIGHT
	jsr	mm_lda_bank
	tay
	lda	#0
	jsr	vtui_gotoxy
	ldy	#XV_DESKTOP_WIDTH
	jsr	mm_lda_bank
	pha
	ldy	#XV_STATUS_COLOR
	jsr	mm_lda_bank
	pha
	dey	;XV_STATUS_DIRTY
	lda	#0
	jsr	mm_sta_bank
	plx
	ply
	lda	#' '
	jsr	vtui_hline
	clc
	rts
.endproc

;*****************************************************************************
;=============================================================================
;-----------------------------------------------------------------------------
;*****************************************************************************
.proc	xv_tick: near
	lda	Vera_Reg_ISR
	and	#$01	; Is this VSYNC
	beq	end	; If not, ignore
	lda	#2
	sta	Vera_Reg_DCBorder
	; Handle VSYNC Interrupt
	dec	jiffiecnt
	bne	:+
	lda	#60
	sta	jiffiecnt
	jsr	update_time
:	jsr	update_desktop
	bcc	done
	jsr	update_statusbar
	bcc	done
done:	stz	Vera_Reg_DCBorder
end:	rts
.endproc

.assert __XVKITVARS_SIZE__ <= 255, error, "XVKITVARS segment is larger than 255 bytes, check zeroing code"
