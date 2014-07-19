.model medium,basic

.data
	screenSeg	dw	0b800h

.code

;================================================================================	
aPageFlip PROC
	; ██████╗  █████╗  ██████╗ ███████╗███████╗██╗     ██╗██████╗ 
	; ██╔══██╗██╔══██╗██╔════╝ ██╔════╝██╔════╝██║     ██║██╔══██╗
	; ██████╔╝███████║██║  ███╗█████╗  █████╗  ██║     ██║██████╔╝
	; ██╔═══╝ ██╔══██║██║   ██║██╔══╝  ██╔══╝  ██║     ██║██╔═══╝ 
	; ██║     ██║  ██║╚██████╔╝███████╗██║     ███████╗██║██║     
	; ╚═╝     ╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚═╝     ╚══════╝╚═╝╚═╝     
                                                            
	; Page flip and vertical sync.
	;
	; Waits for vertical retrace and changes video start offset.

	; Parameter stack offsets
	; Order is inverted from qbasic CALL parameter order

	;00 bp
	;02 Qbasic return segment
	;04 Qbasic return offset

	;06 set page offset
	;08 previous page offset
	;10 hud string offset
	;12 hud string segment
	;14 video wrap offset

	;============================================================================

				push bp
				mov bp,sp

	;---------------------------------------------------------------------------
	; Copy HUD to set page
	;---------------------------------------------------------------------------
				mov dx, [bp + 14]				;DX = Video wrap offset

				mov ds, [bp + 12]
				mov si, [bp + 10]

				mov es, screenSeg				;ES:DI = Page offset * 2
				mov di, [bp + 06]
				shl di, 1
	mov cx, 16									;Hud string = 160 bytes
	copyHud:		
				movsw							;Copy character and attribute
				and di, dx						;Wrap DI with video wrap offset		
				movsw							
				and di, dx						
				movsw							
				and di, dx						
				movsw							
				and di, dx						
				movsw							
				and di, dx						
	loop copyHud

	;---------------------------------------------------------------------------
	; Wait for vertical overscan and retrace.
	;---------------------------------------------------------------------------
				mov dx, 03DAh

	wait1:
				in  al, dx
				and al, 8
	jnz wait1

	wait2:
				in  al, dx
				and al, 8
	jz wait2

	;---------------------------------------------------------------------------
	; Change page offset
	;---------------------------------------------------------------------------
		mov dx, 03d4h 
		mov bx, 03d5h

		mov al, 0dh
		out dx, al
		xchg dx, bx
		mov al, [bp + 06]
		out dx, al

		xchg dx, bx

		mov al, 0ch
		out dx, al
		xchg dx, bx
		mov al, [bp + 07]
		out dx, al

	;---------------------------------------------------------------------------
	; Clear HUD from previous page
	;---------------------------------------------------------------------------
		mov dx, [bp + 14]				;DX = Video wrap offset
		mov di, [bp + 08]				;DI = previous page offset * 2
		shl di, 1
		mov ax, 011DEh					;Clear attribute = 11, character = 222
	mov cx, 16
	clearHud:		
		stosw							;Copy character and attribute
		and di, dx						;Wrap DI with video wrap offset		
		stosw							
		and di, dx						
		stosw							
		and di, dx						
		stosw							
		and di, dx						
		stosw							
		and di, dx						
	loop clearHud

	;============================================================================

	exit:
				pop bp
				retf 10
aPageFlip endp
;================================================================================

public aPageFlip

end