.model medium,basic

.code

;================================================================================	
aKBremove PROC

;------------------------------------------------------------------------------
; Keyboard routine remove and restore.
;
;------------------------------------------------------------------------------

; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 bp
;02 ds
;04 Qbasic return segment
;06 Qbasic return offset

;08 Old offset
;10 Old segment

;------------------------------------------------------------------------------

push ds
push bp
mov bp, sp

mov dx, [bp + 10]
mov ds, dx
mov dx, [bp + 08]

mov ah, 025h
mov al, 09h
int 021h

pop bp
pop ds
retf 4

;------------------------------------------------------------------------------

aKBremove ENDP

public aKBremove

end