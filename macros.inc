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

.endif
