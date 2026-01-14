.segment "MEMMAN"
.include "x16.inc"
.include "x16vision.inc"

.import rem_space
.export check_rem_space

;*****************************************************************************
; Check that a certain amount of memory is available in an allocated bank
;=============================================================================
; Input:	X = ram bank to check
;		X16_Reg_X0L = Amount of bytes needed
;-----------------------------------------------------------------------------
; Returns:	Carry clear on success, else Carry set with errorcode in A
;*****************************************************************************
.proc check_rem_space: near
	cpx	X16_RAMBank_Reg	; If we are already on correct ram bank
	beq	@continue
	lda	X16_RAMBank_Reg	; Save current ram bank and set correct bank
	stx	X16_RAMBank_Reg
	tax
@continue:
	lda	rem_space+1	; If highbyte > 0 then there is room for 256 bytes
	beq	@good
	lda	rem_space	; Else check if lowbyte is larger than required size
	cmp	X16_Reg_X0L
	bcc	@good
	; There is not enough memory available
	cpx	X16_RAMBank_Reg	; If we are already on correct ram bank
	beq	@noswitch
	lda	X16_RAMBank_Reg	; Save current bank and set original bank
	stx	X16_RAMBank_Reg
	tax
@noswitch:
	lda	#XV_NOMEM
	sec
	rts
@good:	clc
	rts
.endproc