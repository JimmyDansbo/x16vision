.ifndef macros_inc
macros_inc=1

.macro DEBUG_STOP
	.byte	$DB
.endmacro

.endif