; This include file can be imported into an assembly project.
; To call any of the routines, switch to the bank where code
; is loaded, then jsr to the routine

xv_initialize	= $A000 ; Initialize the library
xv_setisr	= $A003 ; Install interrupt handler
xv_clearisr	= $A006	; Restore old interrupt handler

xv_bank_offset	= 11	; Offset from the lowram address where the RAM bank is stored

