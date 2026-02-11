; This include file can be imported into an assembly project.
; To call any of the routines, switch to the bank where code
; is loaded, then jsr to the routine

xv_initialize	= $A024 ; Initialize the library
xv_setisr	= $A027 ; Install interrupt handler
xv_clearisr	= $A02A	; Restore old interrupt handler
xv_desktop	= $A02D ; Create the desktop, set screen, charset, background pattern and color
xv_statusbar	= $A030	; Create a status bar
xv_menubar	= $A033 ; Create a menu bar

; Error codes
XV_NOERR			= $00
; Below codes are copied directly from memory manager
XV_ERR_ZERO			= $01	; Zero byte allocation requested
XV_ERR_NOSPACE			= $02	; Not enough available space in bank
XV_ERR_NOHANDLE 		= $03	; No available handles in current bank
XV_ERR_HANDLE_NOTFOUND		= $04	; Requested handle was not found
XV_ERR_CORRUPT_HDR		= $05	; Memory area header corrupt
XV_ERR_MODE_UNSUPPORTED		= $06	; The chosen screen mode is unsupported
; XV specific errors
XV_ERR_CHARSET_UNSUPPORTED	= $07	; The current character set is not recognized as supported
XV_ERR_NO_DESKTOP		= $08

; Memory stuctures
XV_DESKTOP_DIRTY	= 0
XV_DESKTOP_CHAR		= 1
XV_DESKTOP_COL		= 2
XV_DESKTOP_WIDTH	= 3
XV_DESKTOP_HEIGHT	= 4
XV_DESKTOP_Y_START	= 5
XV_DESKTOP_FIRST_WIN	= 6
XV_DESKTOP_ACTIVE_WIN	= 8
XV_MENU_BAR_DIRTY	= 10
XV_MENU_BAR_COLOR	= 11
XV_MENU_BAR_SEL_COLOR	= 12
XV_MENU_BAR_FIRST_ITEM	= 13
XV_STATUS_DIRTY		= 15
XV_STATUS_COLOR		= 16
XV_STATUS_SEL_COLOR	= 17
XV_STATUS_FIRST_ITEM	= 18
XV_DESKTOP_STRUCT_SIZE	= 20

; General information for all elements
XV_ELEM_TYPE		= 0
XV_ELEM_SIZE		= 1
XV_ELEM_NEXT_PTR	= 2
XV_ELEM_NEXT_BANK	= 4

