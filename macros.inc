.ifndef macros_inc
macros_inc=1

.macro DESKTOP_PTR numpulls
	lda	desktop_handle+0
	bne	:+	; If handle is 0, desktop is not allocated yet
	.if !.blank(numpulls)
		.repeat numpulls
			pla
		.endrepeat
	.endif
	lda	#XV_ERR_NO_DESKTOP
	sec
	rts
:	ldy	desktop_handle+1
	jsr	mm_get_ptr
	bcc	:+	; Exit if pointer not found
	.if !.blank(numpulls)
		.repeat numpulls
			ply
		.endrepeat
	.endif
	rts
:	jsr	mm_store_zp1
.endmacro

.macro DATA1_ADDR addr
	lda	#1
	sta	Vera_Reg_Ctrl	; Set ADDRSEL to 1
	lda	#$10|^addr	; Ensure increment is 1 and set VRAM address bit 16
	sta	Vera_Reg_AddrH
	lda	#>addr
	sta	Vera_Reg_AddrM
	lda	#<addr
	sta	Vera_Reg_AddrL
	stz	Vera_Reg_Ctrl	; Set ADDRSEL back to 0
.endmacro

.endif
