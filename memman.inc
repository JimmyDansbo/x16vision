.ifndef memman_inc
memman_inc=1

; This constant decides which ZP address is being used as ZP pointer
; for the lowram functions to load and store values in banked memory
LD_ST_BANK_PTR = X16_PTR_0

.ifndef SKIPIMPORT
.import mm_init_lowram, mm_set_isr, mm_clear_isr, mm_alloc, mm_remaining
.endif

.endif