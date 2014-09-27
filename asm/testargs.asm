.model medium,basic

.data
	msg   		db  "00 $01 $02 $03 $04 $05 $06 $07 $"
		
.code

PROC testArgs
	; Parameter stack offsets
	
	;00 bp
	;02 Qbasic return segment
	;04 Qbasic return offset

	;06 Arg 0
	;08 Arg 1
	;10 Arg 2
	;12 Arg 3

		push bp
		mov bp, sp
		
		mov cx, 0
		
	argLoop:
		
	startTest:
		mov ah, 09				;Print string at DX
		mov dx, OFFSET msg		;Set message string start offset
		
		push bp					;Get argument from stack and add CX
		add bp, 6
		add bp, cx
		
		mov bx, [bp]			;BX = Argument offset
		pop bp
		
		shl bx, 1				;Argument offset * 4
		shl bx, 1
		
		add dx, bx				;Add argument offset to message string offset.
		
		int 21h					;BIOS: print string.
	
	nextTest:
		add cx, 2				;Next argument. (Word)
		cmp cx, 8				;End of list?
		je exit
		jmp argLoop	
			
	exit:
		pop bp
		retf 8
	
testArgs ENDP

public testArgs

end