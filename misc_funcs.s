zp_ptr = $30

; Loading .a from address pointed to by zp_ptr in ram bank stored in .x
.proc lda_bank
phy
ldy RAM_BANK
stx RAM_BANK
lda (zp_ptr)
sty RAM_BANK
ply 
rts
.endproc

.proc ldax_bank
phy 
ldy RAM_BANK
stx RAM_BANK
lda (zp_ptr)
tax 
inc zp_ptr+1
bne:+
inc zp_ptr
:
lda (zp_ptr)
sty RAM_BANK
ply 
rts
.endproc