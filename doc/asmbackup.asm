aVideoDetect PROC
;============================================================================
;
;	Checks for video adapters VGA, EGA, CGA or MONO/OTHER
;	
;	If MONO/OTHER is found, 0 is returned at ES:DI (and the game won't run).
;
;	VGA/EGA/CGA will initialize 40x50 text mode with 8x8 characters.
;
;============================================================================

; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 bp
;02 Qbasic return segment
;04 Qbasic return offset

;06 Video adapter data offset	/	64 bytes of data to accommodate
;08 Video adapter data segment	/	VGA state check and adapter test result.
;
;RETURNS AX = detected adapter
;
;============================================================================
	push bp
	mov bp,sp
	
	push es
	push di
	push ds
	push si
;---------------------------------------------------------------------------
	mov ax, 0F00h					;Get current video mode
	int 10h							;Stored by interrupt to to AL
	
	mov vOldMode, al
;---------------------------------------------------------------------------
	mov es, [bp + 08]				;ES = Write seg (video adapter data)
	mov di, [bp + 06]				;DI = Write ofs (video adapter data)
	push di
	xor ax, ax
	mov cx, 32						;Format string to test for data.
	rep stosw						
	pop di
;---------------------------------------------------------------------------
test_VGA:
	xor ax, ax						;Interrupt call 10h AH=1Bh AL=0 BX=0
	mov ah, 01bh					;Get VGA functionality and state.
	xor bx, bx						;dumps 64 bytes at ES:[DI]
	
	int 10h
		
	cmp al, 01bh					;If AL=1Bh this is a VGA
	je VGAdetected
									;Check through data dump before
									; giving up on VGA:
	mov ds, [bp + 08]				;DS = seg (video adapter data)
	mov si, [bp + 06]				;SI = ofs (video adapter data)
	xor bx, bx
	mov cx, 16
VGAstatedatacheck:
	lodsw							;AX = DS:[SI], SI+2
	or bx, ax						;BX: accumulate any bits from the data dump.
loop VGAstatedatacheck				;If nothing's there, this isn't a VGA.
	
	cmp bx, 0
	jz test_EGA
VGAdetected:
	mov dx, 3						;Save adapter # to DX
	jmp exit
;---------------------------------------------------------------------------
test_EGA:
	xor ax, ax
	xor bx, bx						;Interrupt call 10h:
	mov ah, 12h						;Get EGA information.
	mov bl, 10h						;Returns BL > 4 if not EGA.
									
	int 10h							
									
	cmp bl, 4						;BL =< 4 means we're EGA
	ja test_CGA						;compatible.
	
	mov dx, 2						;Save adapter # to DX
	jmp exit
;---------------------------------------------------------------------------
test_CGA:	
	int 11h							;Interrupt call: Equipment check.
	
	and ax, 30h						;Check bits 4 & 5
	
	cmp ax, 30h						;If bits are on, this is a mono adapter.
	jz detected_MONO
	mov dx, 1						;Save adapter # to DX
	jmp exit
;---------------------------------------------------------------------------
detected_MONO:
	mov dx, 0						;Save adapter # to DX
;---------------------------------------------------------------------------

exit:
	mov ax, dx						;Store detected video adapter to AX
	
	pop si
	pop ds
	pop di
	pop es
	pop bp
	retf 4
;================================================================================	
aVideoDetect ENDP
