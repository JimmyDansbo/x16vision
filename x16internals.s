.include "x16.inc"
.include "macros.inc"
.include "x16vision.inc"
.include "memman.inc"

.export desktop_handle, xv_tick
.export scr_width, scr_height, petcp

; Imports from vtui.s
.import vtui_setbank, vtui_setstride, vtui_setdecr, vtui_clrscr
.import vtui_gotoxy, vtui_plotchar, vtui_scanchar, vtui_hline
.import vtui_vline, vtui_fillbox, vtui_saverect, vtui_restrect
.import vtui_getbank, vtui_getstride, vtui_getdecr
.import vtui_width, vtui_height

.segment "XVKITVARS"
desktop_handle:	.res 2
petcp:		.res 1

.segment "XVKITLIB"
scr_width:	.byte 80,80,40,40,40,20,20,22,64,64,32,32
scr_height:	.byte 60,30,60,30,15,30,15,23,50,25,50,25

; TR=TopRight, TL=TopLeft, BR=BottomRight, BL=BottomLeft, HO=Horizontal, VE=Vertical
; TT=TopT, BT=BottomT, LT=LeftT, RT=RightT, CR=Cross
;		      TR   TL   BR   BL   HO   VE   TT	 BT   LT   RT	CR
cp437_charset:	.byte $AA, $A9, $D9, $C0, $C4, $B3, $C2, $C1, $C3, $B4, $C5
pet_charset:	.byte $6E, $70, $7D, $6D, $40, $42, $72, $71, $6B, $73, $5B

;*****************************************************************************
; Redraw menu bar if it is needed (dirty)
;=============================================================================
; Output:	.C clear if update work has been done
;-----------------------------------------------------------------------------
; Preserves:	Nothing
;*****************************************************************************
.proc update_menubar: near
	DESKTOP_PTR
	ldy	#XV_MENU_BAR_DIRTY
	jsr	mm_lda_bank
	bne	:+
	sec
	rts
:	lda	#0
	ldy	#0
	jsr	vtui_gotoxy
	ldy	#XV_DESKTOP_WIDTH
	jsr	mm_lda_bank
	pha		; Width
	ldy	#XV_MENU_BAR_COLOR
	jsr	mm_lda_bank
	pha		; Color
	dey	;XV_MENU_BAR_DIRTY
	lda	#0
	jsr	mm_sta_bank
	plx		; Color
	ply		; Width
	lda	#' '
	jsr	vtui_hline
	clc
	rts
.endproc

;*****************************************************************************
; Redraw status bar if it is needed (dirty)
;=============================================================================
; Output:	.C clear if update work has been done
;-----------------------------------------------------------------------------
; Preserves:	Nothing
;*****************************************************************************
.proc update_statusbar: near
	DESKTOP_PTR
	ldy	#XV_STATUS_DIRTY
	jsr	mm_lda_bank
	bne	:+
	sec
	rts
:	ldy	#XV_DESKTOP_HEIGHT
	jsr	mm_lday_bank
	cpy	#1
	bne	:+
	inc
:	tay
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
; Redraw desktop if it is needed (dirty)
;=============================================================================
; Output:	.C clear if update work has been done
;-----------------------------------------------------------------------------
; Preserves:	Nothing
;*****************************************************************************
.proc update_desktop: near
	; Set memory bank and ZP pointer
	DESKTOP_PTR
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
; Update dirty elements
;=============================================================================
; All registers and ZP1 are preserved by the calling ISR
;-----------------------------------------------------------------------------
; Depends:	Needs access to the global jiffiecnt variable
;*****************************************************************************
.proc	xv_tick: near
	lda	Vera_Reg_ISR
	and	#$01	; Is this VSYNC
	beq	end	; If not, ignore
	lda	#2
	sta	Vera_Reg_DCBorder
	; Handle VSYNC Interrupt
	jsr	update_desktop
	bcc	done
	jsr	update_statusbar
	bcc	done
	jsr	update_menubar
	bcc	done
done:	stz	Vera_Reg_DCBorder
end:	rts
.endproc
