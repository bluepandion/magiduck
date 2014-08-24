;------------------------------------------------------------------------------
;
; Keyboard routine v3
;
; Modified after Jim Leonard's article for PC/XT machines.
;
;------------------------------------------------------------------------------

push ds
push ax
push bx

mov bx, 1234h							;This number will be overwritten in
mov ds, bx								;Qbasic. Set our write segment
										;to the Keyboard array.
mov bx, 4321h							;And an offset value too
										
xor ah, ah								;AH = 0
in al, 060h								;AL = Read from port 60

CMP al, 07f								;Is it < 127?
JA release
	shl al, 1							;AL * 2
	add bx, ax							;BX = Keyboard array offset + Key offset (AX)
	mov al, 1							;Write a one to enable key
	mov ds:[bx], al
	JMP exit							;We're done, exit.
release:
										;Key released.
	and al, 07f							;Remove higheset bit of AL
	shl al, 1							;AL * 2
	add bx, ax							;BX = Keyboard array offset + Key offset (AX)
	mov al, 0
	mov ds:[bx], al						;Write a zero to disable key
	
exit:

in al, 061h								;Reset keyboard, send EOI to XT keyboard
mov ah, al								;Store value to AH
or al, 080h								;Set Bit 7 to acknowledge scancode.
out 061h, al
xchg ah, al
out 061h, al

mov al, 020h							;Send EOI to master PIC
out 020h, al

pop bx									;Pop registers
pop ax
pop ds

iret									;Exit interrupt
