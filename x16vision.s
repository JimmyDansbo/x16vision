.include "x16.inc"

.import __XVKIT_LOWRAM_SIZE__

X16VISION_VERSION = $0001

.segment "JUMPTABLE"
	jmp	xv_initialize	; $A000

.segment "XVKITBSS"
_XV_BSS_START = *
temp:	.res 1
_XV_BSS_END = *

.segment "XVKITLIB"

.proc	xv_initialize: near
	rts
.endproc

.segment "XVKIT_LOWRAM"
_isr:
	rts


.assert __XVKIT_LOWRAM_SIZE__ <= 255, error, "XVKIT_LOWRAM segment may not be larger than 255 bytes"