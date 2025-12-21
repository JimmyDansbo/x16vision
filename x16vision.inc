; This include file can be imported into an assembly project.
; To call any of the routines, switch to the bank where code
; is loaded, then jsr to the routine

xv_initialize	= $A000 ; Initialize the library
xv_setisr	= $A003 ; Install interrupt handler
xv_clearisr	= $A006	; Restore old interrupt handler
xv_desktop	= $A009 ; Create the desktop, set screen, charset, background pattern and color
xv_statusbar	= $A00C	; Create a status bar

xv_bank_offset	= 11	; Offset from the lowram address where the RAM bank is stored

; Error codes
XV_NOERR	= $00
XV_UNINIT	= $01	; Library has not yet been initialized
XV_DONE_ALREADY	= $02	; Function has already been run. (must be 2 for gate and ungate of isr functions to work)
XV_WRONGROM	= $03	; Unsupported ROM version
XV_PRERELROM	= $04	; Warning about pre-release ROM version
XV_NOMEM	= $05	; Not enough available memory

; Memory stuctures
XV_DESKTOP_CHAR		= 0
XV_DESKTOP_COL		= 1
XV_DESKTOP_WIDTH	= 2
XV_DESKTOP_HEIGHT	= 3
XV_DESKTOP_STRUCT_SIZE	= 4

; General information for all elements
XV_ELEM_TYPE		= 0
XV_ELEM_SIZE		= 1
XV_ELEM_NEXT_PTR	= 2
XV_ELEM_NEXT_BANK	= 4

