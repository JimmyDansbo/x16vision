; This include file can be imported into an assembly project.
; To call any of the routines, switch to the bank where code
; is loaded, then jsr to the routine

xv_initialize	= $A000 ; Initialize the library
xv_setisr	= $A003 ; Install interrupt handler
xv_clearisr	= $A006	; Restore old interrupt handler
xv_desktop	= $A009 ; Create the desktop, set screen, charset, background pattern and color

xv_bank_offset	= 11	; Offset from the lowram address where the RAM bank is stored

XV_ERR_NOERR		= $00
XV_ERR_WRONGROM		= $01
XV_ERR_PRERELROM	= $02
