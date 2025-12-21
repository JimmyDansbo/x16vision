.ifndef macros_inc
macros_inc=1

.macro SAVE_PTR ptr
	lda	ptr
	sta	preserve_ptr
	lda	ptr+1
	sta	preserve_ptr+1
.endmacro

.macro STACK_SAVE_PTR ptr
	lda	ptr
	pha
	lda	ptr+1
	pha
.endmacro

.macro RESTORE_PTR ptr
	lda	preserve_ptr
	sta	ptr
	lda	preserve_ptr+1
	sta	ptr+1
.endmacro

.macro STACK_RESTORE_PTR ptr
	pla
	sta	ptr+1
	pla
	sta	ptr
.endmacro

.macro DEBUG_STOP
	.byte	$DB
.endmacro

.macro GATE_THIS_FUNCTION
	lda	#XV_UNINIT
	sec
	rts
.endmacro

.macro STA_PTR ptr, offset
	ldy	#offset
	sta	(ptr),y
.endmacro

.macro LDA_PTR ptr, offset
	ldy	#offset
	lda	(ptr),y
.endmacro

.macro SUB val, imm
	sec
	lda	val
	sbc	#imm
	sta	val
	lda	val+1
	sbc	#0
	sta	val+1
.endmacro

.endif