.include "x16.inc"
.include "macros.inc"
.include "vtui.inc"
.include "x16vision.inc"

.import __XVKIT_LOWRAM_SIZE__, __XVKITBSS_SIZE__, __XVKITBSS_LOAD__

X16VISION_VERSION	= $0001
MINIMUM_ROMVERSION	= 48

.segment "JUMPTABLE"
	jmp	_xv_initialize	; $A000
	jmp	_xv_setisr	; $A003
	jmp	_xv_clearisr	; $A006
	jmp	_xv_desktop	; $A009
	jmp	_xv_statusbar	; $A00C

; Internal jump table into lowram functions
; The bank-load and store functions use X16_PTR_0 for the address and X for bank
lda_bank:		; Return byte in A
	jmp	$0000
lday_bank:		; Return lowbyte in A, highbyte in Y
	jmp	$0000
ldyxa_bank:		; Return lowbyte in Y, midbyte in X , highbyte in A
	jmp	$0000
sta_bank:		; Store value in A
	jmp	$0000
stay_bank:		; Store A to lowbyte, Y to highbyte
	jmp	$0000

check_bank_remspace:
	jmp	$0000

.segment "XVKITBSS"

; Lowram address provided by the user to copy routines to.
lowram_addr:	.res 2
; Object handled by library are in a linked list, this points to the start of the list.
init_obj_addr:	.res 2
init_obj_bank:	.res 1
; This points to the latest object for ease of adding objects.
last_obj_addr:	.res 2
last_obj_bank:	.res 1
; Space reserved for saving ZP pointers before using them
preserve_ptr:	.res 4
; Space reserved for the desktop information
desktop:	.res XV_DESKTOP_STRUCT_SIZE
; Variable holding amount of free space in the bank
rem_space:	.res 2

START_OF_LINKED_LIST = *
.segment "XVKITLIB"
OP_BRA=$80	; Opcode for BRA
OP_LDA_IMM=$A9	; Opcode for LDA #

;*****************************************************************************
;=============================================================================
;*****************************************************************************
.proc	xv_tick: near
	; Check if address is all 0s
	lda	init_obj_addr
	ora	init_obj_addr+1
	beq	@end
	; Here we have an address for an object
	SAVE_PTR X16_PTR_0

	RESTORE_PTR X16_PTR_0
@end:	rts
.endproc

;*****************************************************************************
;
;=============================================================================
; Inputs:	placement = top or bottom
;		text alignment right, left, center
;		color
;		highlight color
;		string length
;		pointer to string
;		bank of string
;*****************************************************************************
.proc	_xv_statusbar: near
	GATE_THIS_FUNCTION

	rts
.endproc

;*****************************************************************************
; Print a string. If pointer to string is $A000 or higher, RAM bank must be
; specified.
; Function expects stride to be set to 2 so no color information needed
;=============================================================================
; Inputs:	X16_PTR_0 = pointer to string
;		Y length of string
;		X bank, if in banked memory
; Uses:		A
;*****************************************************************************
.proc	printstr: near
	lda	X16_Reg_X0L
	pha
	sty	X16_Reg_X0L
	ldy	#0
@loop:	cpy	X16_Reg_X0L
	beq	@end
	lda	X16_PTR_0+1
	cmp	#$A0
	bcs	@banked
	lda	(X16_PTR_0),y
	bra	:+
@banked:
	jsr	lda_bank
:	sta	Vera_Reg_Data0
	iny
	bra	@loop
end:	pla
	sta	X16_Reg_X0L
	rts
.endproc

;*****************************************************************************
; Check that a certain amount of memory is available either in XV bank or in
; an allocated bank
;=============================================================================
; Input:	X16_Reg_X0L = Bytes of memory needed
; (optional)	X16_Reg_X0H = RAM Bank number to check for available space
;                             skipped if value of register = 0
;-----------------------------------------------------------------------------
; Returns:	Carry clear on success, else Carry set with errorcode in A
;*****************************************************************************
.proc	check_remaining_space: near
	lda	rem_space+1
	bne	@good
	lda	rem_space
	cmp	X16_Reg_X0L
	bcc	@good
	; There is not enough remaining space in XV bank
	lda	X16_Reg_X0H	; Check that a RAM bank has been specified
	beq	@nobank
	jmp	check_bank_remspace
@nobank:
	lda	#XV_NOMEM
	sec
	rts
@good:	clc
	rts
.endproc

;*****************************************************************************
; Create, draw and save information about the desktop element which must be
; the first element created by the library
;=============================================================================
; Input: Screen Mode	= R0L
;	 Character Set	= R0H
;	 Output mode	= R1L
;	 Background char= A
;	 Color		= X
;*****************************************************************************
.proc	_xv_desktop: near
	GATE_THIS_FUNCTION
	sta	desktop+XV_DESKTOP_CHAR
	stx	desktop+XV_DESKTOP_COL
	
	lda	X16_Reg_R0L	; Load Screen Mode
	clc
	jsr	X16_Kernal_screen_mode
	sec
	jsr	X16_Kernal_screen_mode
	sty	desktop+XV_DESKTOP_HEIGHT
	stx	desktop+XV_DESKTOP_WIDTH

	lda	X16_Reg_R0H	; Load character set
	jsr	X16_Kernal_screen_set_charset
	lda	X16_Reg_R1L	; Load output mode
	and	#$0F		; Ensure only low nibble is set
	ora	Vera_Reg_DCVideo
	sta	Vera_Reg_DCVideo

	lda	#1
	jsr	vtui_setstride
	clc
	jsr	vtui_setdecr
	lda	desktop+XV_DESKTOP_CHAR
	ldx	desktop+XV_DESKTOP_COL
	jsr	vtui_clrscr
	rts
;************************************************
; Saved for later
;************************************************
	pha			; Save character on stack temporarily

	lda	#XV_DESKTOP_STRUCT_SIZE
	sta	X16_Reg_X0L
	stz	X16_Reg_X0H
	jsr	check_remaining_space
	bcc	:+
	rts
:	SAVE_PTR X16_PTR_0
	lda	#<START_OF_LINKED_LIST
	sta	init_obj_addr	; Use ZP pointer to point to initial element
	sta	last_obj_addr
	sta	X16_PTR_0
	lda	#>START_OF_LINKED_LIST
	sta	init_obj_addr+1
	sta	last_obj_addr+1
	sta	X16_PTR_0+1
	lda	X16_RAMBank_Reg
	sta	init_obj_bank
	sta	last_obj_bank
	pla			; Restore character from stack
	phy			; Save Y as it is used to index into structure

	STA_PTR X16_PTR_0, XV_DESKTOP_CHAR
	txa
	STA_PTR X16_PTR_0, XV_DESKTOP_COL
	lda	X16_Reg_R0L	; Load Screenmode
	clc
	jsr	X16_Kernal_screen_mode
	sec
	jsr	X16_Kernal_screen_mode
	tya
	STA_PTR X16_PTR_0, XV_DESKTOP_HEIGHT
	txa
	STA_PTR X16_PTR_0, XV_DESKTOP_WIDTH

	lda	X16_Reg_R0H	; Load character set
	jsr	X16_Kernal_screen_set_charset
	lda	X16_Reg_R1L	; Load output mode
	and	#$0F		; Ensure only low nibble is set
	ora	Vera_Reg_DCVideo
	sta	Vera_Reg_DCVideo

	lda	#XV_DESKTOP_STRUCT_SIZE
	sta	(X16_PTR_0)
	lda	#0
;	STA_PTR	X16_PTR_0, XV_DESKTOP_NEXTBANK
;	STA_PTR X16_PTR_0, XV_DESKTOP_NEXT+0
;	STA_PTR X16_PTR_0, XV_DESKTOP_NEXT+1

	SUB	rem_space, XV_DESKTOP_STRUCT_SIZE

	lda	#1
	jsr	vtui_setstride
	clc
	jsr	vtui_setdecr
	LDA_PTR X16_PTR_0, XV_DESKTOP_COL
	tax
	LDA_PTR X16_PTR_0, XV_DESKTOP_CHAR
	jsr	vtui_clrscr
	pha
	RESTORE_PTR X16_PTR_0
	pla
	ply
	rts
;*********************************************
.endproc

;*****************************************************************************
; Enable the ISR routine, but first ensure that it jumps to the original
; interrupt routine upon exit
;=============================================================================
; Preserves X register
;*****************************************************************************
.proc	_xv_setisr: near
	GATE_THIS_FUNCTION

	; Ungate clearisr function and gate this one
	lda	#OP_BRA
	sta	_xv_clearisr
	lda	#OP_LDA_IMM
	sta	_xv_setisr

	; Save ZP ptr to stack
	SAVE_PTR X16_PTR_0

	; Set ZP ptr to start of lowram isr
	lda	lowram_addr
	sta	X16_PTR_0
	lda	lowram_addr+1
	sta	X16_PTR_0+1
	; Set Y to offset of _old_isr
	ldy	#(_end_isr-_isr-2)

	; Save old interrupt vector
	lda	X16_Vector_IRQ
	sta	(X16_PTR_0),y
	iny
	lda	X16_Vector_IRQ+1
	sta	(X16_PTR_0),y

	; Restore ZP ptr from stack
	RESTORE_PTR X16_PTR_0

	; Tell VERA that we want VSYNC interrupts (should already be enabled)
	lda	Vera_Reg_IEN
	ora	#$01
	sta	Vera_Reg_IEN

	; Install new interrupt vector
	sei	; Disable interrupts
	lda	lowram_addr
	sta	X16_Vector_IRQ
	lda	lowram_addr+1
	sta	X16_Vector_IRQ+1
	cli	; Enable interrupts
	rts
.endproc

;*****************************************************************************
; Disable the ISR routine by reinstalling the original IRQ vector
;=============================================================================
; Preserves X register
;*****************************************************************************
.proc	_xv_clearisr: near
	GATE_THIS_FUNCTION

	; Ungate setisr function and gate this one
	lda	#OP_BRA
	sta	_xv_setisr
	lda	#OP_LDA_IMM
	sta	_xv_clearisr

	; Save ZP ptr to stack
	SAVE_PTR X16_PTR_0

	; Set ZP ptr to start of lowram isr
	lda	lowram_addr
	sta	X16_PTR_0
	lda	lowram_addr+1
	sta	X16_PTR_0+1
	; Set Y to offset of _old_isr
	ldy	#(_end_isr-_isr-2)
	
	sei	; Disable interrupts
	lda	(X16_PTR_0),y
	sta	X16_Vector_IRQ
	iny
	lda	(X16_PTR_0),y
	sta	X16_Vector_IRQ+1
	cli	; Enable interrupts

	; Restore ZP ptr from stack
	RESTORE_PTR X16_PTR_0
	rts
.endproc

;*****************************************************************************
; Reads out the ROM version from address $FF80 in ROM bank 0
;=============================================================================
; Preserves: X and Y
; Returns: current ROM version in A
;*****************************************************************************
.proc	getromver: near
	lda	X16_ROMBank_Reg	; If we are already in ROM Bank 0
	bne	:+
	lda	$FF80		; Read ROM version and return
	rts
:	phy			; Else preserve Y and switch ROM bank
	ldy	X16_ROMBank_Reg
	stz	X16_ROMBank_Reg
	lda	$FF80
	sty	X16_ROMBank_Reg
	ply			; Restore Y
	pha	; Push and Pull A to ensure N flag is correct
	pla
	rts
.endproc

;*****************************************************************************
; Zeroes out variable space, copies functions to lowram and ensures ISR loads 
; correct RAM bank before calling xv_tick to actually handle the interrupt.
;=============================================================================
; Arguments: A and X are low- and high-byte of lowram address reserved for
;            the x16vision library
;-----------------------------------------------------------------------------
; Preserves: X and Y
; Returns: Carry set on error, A = error code
;*****************************************************************************
.proc	_xv_initialize: near
	sta	lowram_addr	; Save low byte of lowram address to mem
	; Ensure that we are are on a supported ROM
	jsr	getromver
	bmi	@isprerelease
	sta	lowram_addr+1	; Save ROM version temporarily
	lda	#XV_NOERR	; Push NOERR onto stack
	pha
	lda	lowram_addr+1	; Restore ROM version to A
	cmp	#MINIMUM_ROMVERSION
	bcs	@continue
	pla			; clear stack
	lda	#XV_WRONGROM
	sec
	rts
@isprerelease:
	sta	lowram_addr+1	; Save ROM version temporarily
	lda	#XV_PRERELROM	; Push Pre Release ROM warning onto stack
	pha
	lda	lowram_addr+1	; Restore ROM version to A
	eor	#$FF
	cmp	#MINIMUM_ROMVERSION
	bcs	@continue
	pla			; clear stack
	lda	#XV_WRONGROM
	sec
	rts
@continue:
	phy	; Preserve Y
	lda	lowram_addr	; Preserve low byte of lowram address on stack
	pha
	; Save ZP ptr
	SAVE_PTR X16_PTR_0

	; Set ZP ptr to address of variables
	lda	#<__XVKITBSS_LOAD__
	sta	X16_PTR_0
	lda	#>__XVKITBSS_LOAD__
	sta	X16_PTR_0+1
	; Zero out variables
	lda	#0
	ldy	#<(__XVKITBSS_SIZE__-1)
@zloop:
	sta	(X16_PTR_0),y
	dey
	bne	@zloop
	sta	(X16_PTR_0)

	; Calculate the remaining free space in the current bank and store
	; the value in the rem_space variable - this is the amount of
	; space the user has for creating objects
	lda	#<(X16_RAM_Window+X16_RAM_WindowSize-START_OF_LINKED_LIST)
	sta	rem_space
	lda	#>(X16_RAM_Window+X16_RAM_WindowSize-START_OF_LINKED_LIST)
	sta	rem_space+1

	; Write low byte of lowram address to ZP ptr
	pla
	sta	lowram_addr
	sta	X16_PTR_0
	; Save high byte of lowram address and write it to ZP ptr
	stx	lowram_addr+1
	stx	X16_PTR_0+1
	; Save current RAM bank, this is where x16vision library is loaded
	lda	X16_RAMBank_Reg
	sta	ISRBANK

	; Copy routines to low ram
	ldy	#(_end_xvkit_lowram-_isr-1) ; Size of xvkit_lowram segment
@loop:
	lda	_isr,y
	sta	(X16_PTR_0),y
	dey
	bne	@loop
	lda	_isr
	sta	(X16_PTR_0)

	; Restore ZP ptr
	RESTORE_PTR X16_PTR_0

	; Calculate addresses of helper functions in lowram and store them
	; in the jumptable
	lda	#(_lda_bank-_isr)
	clc
	adc	lowram_addr
	sta	lda_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	lda_bank+2

	lda	#(_lday_bank-_isr)
	clc
	adc	lowram_addr
	sta	lday_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	lday_bank+2

	lda	#(_ldyxa_bank-_isr)
	clc
	adc	lowram_addr
	sta	ldyxa_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	ldyxa_bank+2

	lda	#(_sta_bank-_isr)
	clc
	adc	lowram_addr
	sta	sta_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	sta_bank+2

	lda	#(_stay_bank-_isr)
	clc
	adc	lowram_addr
	sta	stay_bank+1
	lda	#0
	adc	lowram_addr+1
	sta	stay_bank+2

	lda	#(_check_bank_remspace-_isr)
	clc
	adc	lowram_addr
	sta	check_bank_remspace+1
	lda	#0
	adc	lowram_addr+1
	sta	check_bank_remspace+2

	jsr	vtui_initialize

	; Ungate library functions
	jsr	ungate

	ply	; Restore Y
	pla	; Get err/warn code from stack
	clc	; Clear carry to to show initialization successfull
	rts
.endproc

.proc	ungate: near
	lda	#OP_BRA
	sta	_xv_setisr
	sta	_xv_desktop
	sta	_xv_statusbar
	lda	#2
	sta	_xv_setisr+1
	sta	_xv_clearisr+1
	sta	_xv_desktop+1
	sta	_xv_statusbar+1

	; _xv_clearisr function is still gated as the BRA opcode has not been
	; written to the start of the function.
	; This means that clearisr reads the error code 2 = XV_DONE_ALREADY
	; into A and returns with Carry set.
	rts
.endproc

.segment "XVKIT_LOWRAM"
;*****************************************************************************
; Ensures the interrupt was generated by VSYNC, otherwise it just calls the
; previous interrupt handler.
; If VSYNC generated the interrupt, the current RAM bank is saved, the RAM
; bank where x16vision library is installed is set and a jsr to xv_tick
; function is performed before continuing on to previous interrupt handler
;=============================================================================
; ROM has already preserved registers and previous interrupt handler should
; ensure they are restored. RAM bank is restored before call to original
; interrupt handler
;*****************************************************************************
_isr:
	; Check if interrupt is VSYNC
	lda	Vera_Reg_ISR
	and	#$01
	beq	_old_isr	; If not VSYNC, skip this ISR
	; Save current RAM bank
	lda	X16_RAMBank_Reg
	pha
	; Set RAM bank where x16vision library is loaded
	lda	#$FF	; Modified to correct bank = _isr+11
ISRBANK = * - 1
	sta	X16_RAMBank_Reg
	; Handle tick
	jsr	xv_tick
	; Restore RAM Bank
	pla
	sta	X16_RAMBank_Reg
_old_isr:
	jmp	$FFFF
_end_isr:

;*****************************************************************************
; Load a byte from banked RAM pointed to by ZP pointer X16_PTR_0 in bank
; specified by the value in .X
;=============================================================================
; Arguments: X16_PTR_0, ZP pointer to the memory address that should be read
;            .X holds the RAM bank to read from
;	     .Y holds offset from ZP pointer
;-----------------------------------------------------------------------------
; Preserves: .X and .Y and the RAM bank before call
; Returns: .A contains the value read from memory
;*****************************************************************************
_lda_bank:
	phx			; Preserve bank
	phy			; Preserve offset
	ldy	X16_RAMBank_Reg	; Save current RAM bank
	stx	X16_RAMBank_Reg	; Set new RAM bank
	phy			; Move original RAM bank from Y to X through stack
	plx
	ply			; Pull offset from stack
	lda	(X16_PTR_0),y	; Load value from address pointed to by ZP pointer
	stx	X16_RAMBank_Reg	; Restore original RAM bank
	plx			; Restore RAM bank from caller
	rts

;*****************************************************************************
; Load two bytes from banked RAM pointed to by ZP pointer X16_PTR_0 in bank
; specified by the value in .X
;=============================================================================
; Arguments: X16_PTR_0, ZP pointer to the memory address that should be read
;            .X holds the RAM bank to read from
;-----------------------------------------------------------------------------
; Preserves: X & X16_PTR_0
; Returns: .A contains value from ZP pointer, .Y from ZP pointer + 1
;*****************************************************************************
_lday_bank:
	phx			; Preserve X
	ldy	X16_RAMBank_Reg	; Save original RAM bank
	stx	X16_RAMBank_Reg	; Switch RAM bank
	phy			; Move original RAM bank through stack to X
	plx
	ldy	#1		; Read highbyte into Y
	lda	(X16_PTR_0),y
	tay
	lda	(X16_PTR_0)	; Read lowbyte into A
	stx	X16_RAMBank_Reg	; Restore original RAM bank
	plx			; Restore X
	rts

;*****************************************************************************
; Load three bytes from banked RAM pointed to by ZP pointer X16_PTR_0 in bank
; specified by the value in .X
;=============================================================================
; Arguments: X16_PTR_0, ZP pointer to the memory address that should be read
;            .X holds the RAM bank to read from
;-----------------------------------------------------------------------------
; Preserves: The RAM bank before call
; Returns: .Y=ZP pointer, .X=ZP pointer+1, .A=ZP pointer+2
;*****************************************************************************
_ldyxa_bank:
	ldy	X16_RAMBank_Reg	; Save current RAM bank
	stx	X16_RAMBank_Reg	; Set new RAM bank
	lda	(X16_PTR_0)	; Load value from address pointed to by ZP pointer
	pha			; Save value to stack
	phy			; Save RAM bank to stack
	ldy	#1		; Initialize .Y for reading value through ZP pointer
	lda	(X16_PTR_0),y	; Load next value from address pointed to by ZP pointer with .Y added
	tax			; Store value in .X
	iny
	lda	(X16_PTR_0),y	; Load next value from address pointed to by ZP pointer with .Y added
	ply			; Restore original RAM bank
	sty	X16_RAMBank_Reg
	ply			; Get first read value from stack
	rts

;*****************************************************************************
; Store a byte to banked RAM pointed to by ZP pointer X16_PTR_0 in bank
; specified by the value in .X
;=============================================================================
; Arguments: X16_PTR_0, ZP pointer to the memory address that should be written
;            .X holds the RAM bank to write to
;	     .A holds the value to write
;-----------------------------------------------------------------------------
; Preserves: .A, .X, .Y and the RAM bank before call
; Returns: nothing
;*****************************************************************************
_sta_bank:
	phy			; Preserve .Y on stack
	ldy	X16_RAMBank_Reg	; Save current RAM bank
	stx	X16_RAMBank_Reg	; Set new RAM bank
	sta	(X16_PTR_0)	; Store value in .A to address pointed to by ZP pointer
	sty	X16_RAMBank_Reg	; Restore RAM bank
	ply			; Restore .Y from stack
	rts

;*****************************************************************************
; Store two bytes to banked RAM pointed to by ZP pointer X16_PTR_0 in bank
; specified by the value in .X
;=============================================================================
; Arguments: X16_PTR_0, ZP pointer to the memory address that should be written
;            .X holds the RAM bank to write to
;	     .A & .Y holds the values to write in that order (low-high)
;-----------------------------------------------------------------------------
; Preserves: A, X, Y & The RAM bank before call
; Returns: nothing
;*****************************************************************************
_stay_bank:
	pha			; Preserve A register
	pha			; Preserve low byte
	lda	X16_RAMBank_Reg	; Save current RAM bank in .A
	stx	X16_RAMBank_Reg	; Set the new RAM bank
	plx			; Pull .low byte from stack and store it in .X temporarily
	pha			; Push original RAM bank to stack
	txa			; Move low byte from .X back to .A
	sta	(X16_PTR_0)	; Store low byte to address pointed to by ZP pointer
	tya			; Store high byte to address pointed to by ZP pointer with .Y added
	ldy	#1
	sta	(X16_PTR_0),y
	tay			; Restore high byte to Y
	ldx	X16_RAMBank_Reg	; Restore RAM bank value from the call
	pla			; Restore RAM bank to original
	sta	X16_RAMBank_Reg
	pla			; Restore A register
	rts

;*****************************************************************************
; Check that a certain amount of memory is available in an allocated bank
; A bank is allocated by ensuring that the two first bytes in the bank states
; how much memory is free in the bank. A freshly allocated RAM bank would 
; contain $FE $1F in address $A000 and $A001 respectively = $1FFE bytes free
;=============================================================================
; Input:	X16_Reg_X0L = Bytes of memory needed
;		X16_Reg_X0H = RAM Bank to check for available space
;-----------------------------------------------------------------------------
; Returns:	Carry clear on success, else Carry set with errorcode in A
; Preserves:	X, Y & X16_Reg_X0
;*****************************************************************************
_check_bank_remspace:
	lda	X16_RAMBank_Reg	; Save current RAM Bank
	pha
	lda	X16_Reg_X0H	; Load new RAM bank
	sta	X16_RAMBank_Reg
	lda	$A001
	bne	@bankgood
	lda	$A000
	cmp	X16_Reg_X0L
	bcc	@bankgood
	; There is not enough remaining space in the bank
	pla			; Restore RAM bank
	sta	X16_RAMBank_Reg
	lda	#XV_NOMEM
	sec
	rts
@bankgood:
	pla			; Restore RAM Bank
	sta	X16_RAMBank_Reg
	clc
	rts

_end_xvkit_lowram:

.assert __XVKIT_LOWRAM_SIZE__ <= 255, error, "XVKIT_LOWRAM segment may not be larger than 255 bytes"
.assert __XVKITBSS_SIZE__ <= 255, error, "XVKITBSS segment is larger than 255 bytes, check zeroing code"