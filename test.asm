!cpu w65c02
!src "../cx16stuff/cx16.inc"
!src "x16vision.inc"

+SYS_LINE main

X16_RAMBank_Reg		= $00
X16_PTR_0		= $30


xv_name:	!text "x16vision.bin"
end_xv_name:

main:
	lda	#0
	sta	$01

	lda	#3		; Set CP437 charset
	jsr	$FF62

	lda	#$10
	sta	X16_RAMBank_Reg
	jsr	load_headerless
	stx	$2A		; Store next free address in ZP
	sty	$2B
	; Store lowram address in ZP
	lda	#<library_lowram
	sta	$28
	lda	#>library_lowram
	sta	$29
	lda	#$28		; ZP1 is going to be $28 & $29
	ldy	#$2A		; ZP2 is going to be $2A & $2B
	ldx	#$10

	jsr	xv_initialize
	lda	#$69	; Character
	ldx	#$E1	; Color
	ldy	#0	; Mode
	jsr	xv_desktop

	; Show border by setting hstop lower than usual
	ldy	$9F25
	lda	#$02
	sta	$9F25
	lda	#$9F
	sta	$9F2A
	sty	$9F25

	lda	#$F0	; Normal color
	ldy	#$F2	; highlight color

	jsr	xv_statusbar
	
	lda	#$C0	; Normal color
	ldy	#$C2	; highlight color

	jsr	xv_menubar

	jsr	bleh

-	wai
	bra	-

	lda	#4
	sta	$01
	rts

library_lowram:	!fill 256, $00

mystr		!fill 20, $00

load_headerless:
	lda	#1
	ldx	#8	; Device
	ldy	#2	; Headerless
	jsr	SETLFS
	lda	#(end_xv_name-xv_name)
	ldx	#<xv_name
	ldy	#>xv_name
	jsr	SETNAM
	lda	#0	; Load, not verify
	ldx	#<$A000
	ldy	#>$A000
	jsr	LOAD
	rts


	