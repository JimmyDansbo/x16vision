; This include file can be imported into an assembly project.
; To call any of the routines, switch to the bank where code
; is loaded, then jsr to the routine

xv_initialize	= $A024 ; Initialize the library
xv_setisr	= $A027 ; Install interrupt handler
xv_clearisr	= $A02A	; Restore old interrupt handler
xv_desktop	= $A02D ; Create the desktop, set screen, charset, background pattern and color
xv_statusbar	= $A030	; Create a status bar

; Error codes
XV_NOERR		= $00
; Below codes are copied directly from memory manager
XV_ERR_ZERO		= $01	; Zero byte allocation requested
XV_ERR_NOSPACE		= $02	; Not enough available space in bank
XV_ERR_NOHANDLE 	= $03	; No available handles in current bank
XV_ERR_HANDLE_NOTFOUND	= $04	; Requested handle was not found
XV_ERR_CORRUPT_HDR	= $05	; Memory area header corrupt
XV_ERR_MODE_UNSUPPORTED	= $06	; The chosen screen mode is unsupported

XV_UNINIT	= $01	; Library has not yet been initialized
XV_DONE_ALREADY	= $02	; Function has already been run. (must be 2 for gate and ungate of isr functions to work)
XV_WRONGROM	= $03	; Unsupported ROM version
XV_PRERELROM	= $04	; Warning about pre-release ROM version


; Memory stuctures
XV_DESKTOP_CHAR		= 0
XV_DESKTOP_COL		= 1
XV_DESKTOP_WIDTH	= 2
XV_DESKTOP_HEIGHT	= 3
XV_DESKTOP_FIRST_WIN	= 4
XV_DESKTOP_ACTIVE_WIN	= 6
XV_DESKTOP_MENU_BAR	= 8
XV_DESKTOP_STATUS_BAR	= 10
XV_DESKTOP_STRUCT_SIZE	= 12

; General information for all elements
XV_ELEM_TYPE		= 0
XV_ELEM_SIZE		= 1
XV_ELEM_NEXT_PTR	= 2
XV_ELEM_NEXT_BANK	= 4

