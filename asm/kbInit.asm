.model medium,basic

.code

;================================================================================	
aKBinit PROC

;------------------------------------------------------------------------------
;
; Keyboard init v2
;
;------------------------------------------------------------------------------

; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 ds
;02 bp
;04 Qbasic return segment
;06 Qbasic return offset

;08 New offset
;10 New segment
;12 Old offset
;14 Old segment

;------------------------------------------------------------------------------
push bp
push ds

mov bp, sp

mov ah, 035h		;Subfunction 35h: Get current interrupt handler address
mov al, 09h			;Subfunction 09h: Get address of int 09h (keyboard handler)
int 021h			;Call interrupt 21h

mov di, bx			;Set read offset to BX (address returned by interrupt)
					;ES was set by interrupt as well.

mov bx, [bp + 14]	;Save old segment to our parameter
mov [bx], es

mov bx, [bp + 12]	;Save old offset to our parameter
mov [bx], di

mov dx, [bp + 10]	;Set read segment to New keyboard routine parameter
mov ds, dx			
mov dx, [bp + 8]	;DX = New keyboard routine offset

					;Pass DS:DX to the interrupt
mov ah, 025h		;Subfunction 25h: Change interrupt handler address
mov al, 09h			;Subfunction 09h
int 021h

pop ds
pop bp

retf 8

aKBinit ENDP
;------------------------------------------------------------------------------

public aKBinit

end