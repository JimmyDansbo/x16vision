.ifndef memman_inc
memman_inc=1

; This constant decides which ZP address is being used as ZP pointer
; for the lowram functions to load and store values in banked memory
LD_ST_BANK_PTR = X16_PTR_0

.ifndef SKIPIMPORT
	.import init_lowram, set_banked_isr, clear_banked_isr, check_space, allocate_space
.endif

.endif