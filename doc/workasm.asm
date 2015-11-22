aVideoDetect PROC
;============================================================================
;
;	Checks for video adapters VGA, EGA, CGA or MONO/OTHER
;	
;	If MONO/OTHER is found, 0 is returned at ES:DI (and the game won't run).
;
;	VGA/EGA/CGA will initialize 40x50 text mode with 8x8 characters.
;
;	MCGA, PCJR and TANDY detection is based on PAKUPAKU source code
;	by Jason M. Knight.
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
; 7	vga
; 6	mcga
; 5 ega
; 4	pcjr
; 3 tandy 1000
; 2 tandy SLTL
; 1	cga
; 0 mono
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
	je test_MCGA
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

test_MCGA:							;Test for MCGA to be sure.
	xor  bl,bl        				;null BL so it's set up for non-PS/2 
	mov  ax, 1A00h
	int  10h
	cmp  bl, 0Ah       				;MCGA returns $0A..$0C 
	jb   VGAdetected
	cmp  bl, 0Ch
	jg   VGAdetected
	mov  dx, 6
	jmp exit
VGAdetected:
	mov dx, 7						;Save adapter # to DX
	jmp exit
;---------------------------------------------------------------------------
test_EGA:
	xor ax, ax
	xor bx, bx						;Interrupt call 10h:
	mov ah, 12h						;Get EGA information.
	mov bl, 10h						;Returns BL > 4 if not EGA.
									
	int 10h							
									
	cmp bl, 4						;BL < 5 means we're EGA
	ja test_CGA						;compatible.
	
	mov dx, 5						;Save adapter # to DX
	jmp exit
;---------------------------------------------------------------------------
test_PCJR:
	mov ax, 0FFFFh
	mov es, ax
	mov di, 000Eh     				;second to last byte PCjr/Tandy BIOS info area
	mov al, 0FDh      				;ends up 0xFD only on the Jr.
	cmp es:[di], al
	jne test_TANDY
	mov dx, 4						;Save adapter # to DX
	jmp exit
;---------------------------------------------------------------------------
test_TANDY:
	mov al, 0FFh       				;all tandy's return 0xFF here
	cmp es:[di], al
	jne test_CGA
	mov ax, 0FC00h
	mov es, ax
	xor di, di
	mov al, 21h
	cmp es:[di], al
	jne test_CGA
	mov ah, 0C0h       				;test for SL/TL
	int 15h          				;Get System Environment
	jnc @tandySLTL     				;early Tandy's leave the carry bit set, TL/SL does not
	mov dx, 3						;Save adapter # to DX (tandy 1000)
	jmp exit
detected_tandySLTL:					
	mov dx, 2						;Save adapter # to DX (tandy SLTL)
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