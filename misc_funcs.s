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

; Loading .a and .x from address pointed to by zp_ptr in ram bank stored in .x
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

.proc ldaxy_bank
ldy RAM_BANK
phy
stx RAM_BANK
ldy #0
lda (zp_ptr),y
tax
iny 
lda (zp_ptr),y
sta tmp_var
iny 
lda (zp_ptr),y
ldy tmp_var
rts
tmp_var: .byte $00
.endproc