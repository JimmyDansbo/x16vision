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

.endif