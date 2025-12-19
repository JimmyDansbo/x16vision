.segment "VTUI"
; ******************************* Functions ***********************************
VTUI_LIB=VTUI+2

vtui_initialize	= VTUI_LIB+0
vtui_screenset	= VTUI_LIB+2
vtui_setbank	= VTUI_LIB+5
vtui_setstride	= VTUI_LIB+8
vtui_setdecr	= VTUI_LIB+11
vtui_clrscr	= VTUI_LIB+14
vtui_gotoxy	= VTUI_LIB+17
vtui_plotchar	= VTUI_LIB+20
vtui_scanchar	= VTUI_LIB+23
vtui_hline	= VTUI_LIB+26
vtui_vline	= VTUI_LIB+29
vtui_printstr	= VTUI_LIB+32
vtui_fillbox	= VTUI_LIB+35
vtui_pet2scr	= VTUI_LIB+38
vtui_scr2pet	= VTUI_LIB+41
vtui_border	= VTUI_LIB+44
vtui_saverect	= VTUI_LIB+47
vtui_restrect	= VTUI_LIB+50
vtui_inputstr	= VTUI_LIB+53
vtui_getbank	= VTUI_LIB+56
vtui_getstride	= VTUI_LIB+59
vtui_getdecr	= VTUI_LIB+62

; *****************************************************************************
; Use KERNAL API to set screen to 80x60 or 40x30 or swap between them.
; *****************************************************************************
; INPUT:		.A = Screenmode ($00, $02 or $FF)
; USES:			.A, .X & ,Y
; RETURNS:		.C = 1 in case of error.
; *****************************************************************************
.macro VTUI_SCREENSET
	jsr	vtui_screenset
.endmacro

; *****************************************************************************
; Set VERA bank (High memory) without touching anything else
; *****************************************************************************
; INPUTS:	.C = Bank number, 0 or 1
; USES:		.A
; *****************************************************************************
.macro VTUI_SETBANK
	jsr	vtui_setbank
.endmacro

; *****************************************************************************
; Get current VERA bank (high memory bit)
; *****************************************************************************
; USES:		.A
; RETURNS:	.C = Bank number, 0 or 1
; *****************************************************************************
.macro VTUI_GETBANK
	jsr	vtui_getbank
.endmacro

; *****************************************************************************
; Set the stride without changing other values in VERA_ADDR_H
; *****************************************************************************
; INPUT:		.A = Stride value
; USES:			r0l
; *****************************************************************************
.macro VTUI_SETSTRIDE
	jsr	vtui_setstride
.endmacro

; *****************************************************************************
; Get current VERA stride value
; *****************************************************************************
; RETURNS:		.A = stride value
; *****************************************************************************
.macro VTUI_GETSTRIDE
	jsr	vtui_getstride
.endmacro

; *****************************************************************************
; Set the decrement value without changing other values in VERA_ADDR_H
; *****************************************************************************
; INPUT:		.C (1 = decrement, 0 = increment)
; USES:			.A
; *****************************************************************************
.macro VTUI_SETDECR
	jsr	vtui_setdecr
.endmacro

; *****************************************************************************
; Get the current VERA decrement value
; *****************************************************************************
; USES:			.A
; RETURNS:		.C (1 = decrement, 0 = increment)
; *****************************************************************************
.macro VTUI_GETDECR
	jsr	vtui_getdecr
.endmacro

; *****************************************************************************
; Write character and possibly color to current VERA address
; If VERA stride = 1 and decrement = 0, colorcode in X will be written as well.
; *****************************************************************************
; INPUTS:	.A = character
;		.X = bg-/fg-color
; USES:		.A
; *****************************************************************************
.macro VTUI_PLOTCHAR
	jsr	vtui_plotchar
.endmacro

; *****************************************************************************
; Read character and possibly color from current VERA address
; If VERA stride = 1 and decrement = 0, colorcode will be returned in X
; *****************************************************************************
; OUTPUS:	.A = character
;		.X = bg-/fg-color
; USES		.X
; *****************************************************************************
.macro VTUI_SCANCHAR
	jsr	vtui_scanchar
.endmacro

; *****************************************************************************
; Create a horizontal line going from left to right.
; *****************************************************************************
; INPUTS:	.A	= Character to use for drawing the line
;		.Y	= Length of the line
;		.X	= bg- & fg-color
; *****************************************************************************
.macro VTUI_HLINE
	jsr	vtui_hline
.endmacro

; *****************************************************************************
; Create a vertical line going from top to bottom.
; *****************************************************************************
; INPUTS:	.A	= Character to use for drawing the line
;		.Y	= Height of the line
;		.X	= bg- & fg-color
; USES:		r1h & r2h
; *****************************************************************************
.macro VTUI_VLINE
	jsr	vtui_vline
.endmacro

; *****************************************************************************
; Set VERA address to point to specific point on screen
; *****************************************************************************
; INPUTS:	.A = x coordinate
;		.Y = y coordinate
; *****************************************************************************
.macro VTUI_GOTOXY
	jsr	vtui_gotoxy
.endmacro

; *****************************************************************************
; Convert PETSCII codes between $20 and $5F to screencodes.
; *****************************************************************************
; INPUTS:	.A = character to convert
; OUTPUS:	.A = converted character or $56 if invalid input
; *****************************************************************************
.macro VTUI_PET2SCR
	jsr	vtui_pet2scr
.endmacro

; *****************************************************************************
; Convert screencodes between $00 and $3F to PETSCII.
; *****************************************************************************
; INPUTS:	.A = character to convert
; OUTPUS:	.A = converted character or $76 if invalid input
; *****************************************************************************
.macro VTUI_SCR2PET
	jsr	vtui_scr2pet
.endmacro

; *****************************************************************************
; Print PETSCII/Screencode string.
; *****************************************************************************
; INPUTS	.A = Convert string (0 = Convert from PETSCII, $80 = no conversion)
;		r0 = pointer to string
;		.Y = length of string
;		.X  = bg-/fg color (only used if stride=0,decr=0&bank=0)
; USES:		.A, .Y & r1
; *****************************************************************************
.macro VTUI_PRINTSTR
	jsr	vtui_printstr
.endmacro

; *****************************************************************************
; Create a filled box drawn from top left to bottom right
; *****************************************************************************
; INPUTS:	.A	= Character to use for drawing the line
;		r1l	= Width of box
;		r2l	= Height of box
;		.X	= bg- & fg-color
; *****************************************************************************
.macro VTUI_FILLBOX
	jsr	vtui_fillbox
.endmacro

; *****************************************************************************
; Clear the entire screen with specific character and color
; *****************************************************************************
; INPUTS:	.A	= Character to use for filling
;		.X	= bg- & fg-color
; USES:		.Y, r1l & r2l
; *****************************************************************************
.macro VTUI_CLRSCR
	jsr	vtui_clrscr
.endmacro

; *****************************************************************************
; Create a box with a specific border
; *****************************************************************************
; INPUTS:	.A	= Border mode (0-6) any other will default to mode 0
;		r1l	= width
;		r2l	= height
;		.X	= bg-/fg-color
; USES		.Y, r0, r1h & r2h
; *****************************************************************************
.macro VTUI_BORDER
	jsr	vtui_border
.endmacro

; *****************************************************************************
; Copy contents of screen from current position to other memory area in
; either system RAM or VRAM
; *****************************************************************************
; INPUTS:	.C	= VRAM Bank (0 or 1) if .A=$80
;		.A	= Destination RAM (0=system RAM, $80=VRAM)
;		r0 	= Destination address
;		r1l	= width
;		r2l	= height
; USES:		r1h
; *****************************************************************************
.macro VTUI_SAVERECT
	jsr	vtui_saverect
.endmacro

; *****************************************************************************
; Restore contents of screen from other memory area in either system RAM
; or VRAM starting at current position
; *****************************************************************************
; INPUTS:	.C	= VRAM Bank (0 or 1) if .A=$80
;		.A	= Source RAM (0=system RAM, $80=VRAM)
;		r0 	= Source address
;		r1l	= width
;		r2l	= height
; *****************************************************************************
.macro VTUI_RESTRECT
	jsr	vtui_restrect
.endmacro

; *****************************************************************************
; Show a cursor and get a string input from keyboard.
; *****************************************************************************
; INPUTS:	r0 = pointer to buffer to hold string (must be pre-allocated)
;		.Y = maximum length of string
;		.X = color information for input characters
; OUPUTS:	.Y = actual length of input
; USES:		.A & r1
; *****************************************************************************
.macro VTUI_INPUTSTR
	jsr	vtui_inputstr
.endmacro

VTUI:	.incbin	"VTUI1.2.BIN"

VTUI_LIB_END:
