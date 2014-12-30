.model medium,basic

.data
	screenSeg	dw	0b800h

.code

;================================================================================	
aPageFlip PROC
                                                           
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
	mov cx, 16							;Hud string = 160 bytes
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
	
		lodsb							;CL = Video adapter (stored at the
		cmp al, 1
		jne egaVgaPageflip
	
	
	;---------------------------------------------------------------------------
	; CGA Wait for vertical overscan and retrace.
	;---------------------------------------------------------------------------
		mov dx, 03DAh
		mov ah, 8

	cgawait1:							;If currently in retrace, wait
		in  al, dx						;until that's finished.
		and al, ah
	jnz cgawait1								
		
	cgawait2:							;Wait until retrace starts.
		in  al, dx
		and al, ah
	jz cgawait2
	
	;---------------------------------------------------------------------------
	; CGA Change page offset
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
	
	jmp clearHudPrevPage						
	
	;---------------------------------------------------------------------------
	; EGA/VGA Wait for vertical overscan and retrace.
	;---------------------------------------------------------------------------
	egaVgaPageflip:	
		mov dx, 03DAh
		mov ah, 9

	egavgawait1:						;If currently in retrace, wait
		in  al, dx						;until that's finished.
		and al, ah
	jnz egavgawait1								

	;---------------------------------------------------------------------------
	; EGA/VGA Change page offset
	; This needs to be done before vertical retrace with these adapters.
	;---------------------------------------------------------------------------
				
		cli								;Close interrupts
				
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
				
		sti								;Restore interrupts
				
	;---------------------------------------------------------------------------
	
		mov dx, 03DAh
		mov ah, 8

	egavgawait2:						;Wait until retrace starts.
		in  al, dx
		and al, ah
	jz egavgawait2
	
	
	;---------------------------------------------------------------------------
	; Clear HUD from previous page
	;---------------------------------------------------------------------------
	clearHudPrevPage:
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

mov di, bx			;Set write offset to BX (address returned by interrupt)
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
;================================================================================	
aClearList PROC

;---------------------------------------------------------------------------
; Rectangle Character Clear Routine v. 8.1
;
; 40x50 mode drawing. 2 Pixels per byte.
;
; Clears a rectangle area of characters in Video memory.
;
; Uses sprite lists to clear many rectangles within one call.
;
;---------------------------------------------------------------------------
;
; Sprite list format:
; 00: 	Sprite offset		.....	
; 02: 	X
; 04: 	Y					.....		y 255 = END OF LIST
; 06: 	W								w   0 = Skip this sprite
; 08: 	H					.....		

;---------------------------------------------------------------------------
; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 bp
;02 Qbasic return segment
;04 Qbasic return offset

;06 Sprite list offset
;08 Sprite list segment
;10 Character and attribute to clear with
;12 Video buffer offset
;14 Video buffer segment
;16 Video Wrap (CGA = 16384, EGA-> = 32768)

;----------------------------------------------------------------------------

push bp				
mov bp, sp				;Get stack pointer

mov di, [bp + 12]		;Video buffer offset * 2
shl di, 1
mov [bp + 12], di

mov es, [bp + 14]		;ES = video memory write segment (0xB800)

mov ds, [bp + 08]		;DS = sprite List read segment
mov si, [bp + 06]		;SI = sprite List read offset

;============================================================================
; SPRITE LIST READ
;============================================================================
newRectangle:

lodsw					;AX = DS:[SI], [SI] + 2			Skip sprite offset

lodsw					;Sprite X
xchg bx, ax
lodsw					;Sprite Y

cmp ax, 255				;Has list ended?
JNZ continuelist		;
exit:					;----------------------------------------------------
pop bp					;		Pop BP
retf 12					;		EXIT
						;----------------------------------------------------

continuelist:
xchg ah, al				;AX * 256
or bx, ax				;BH = Sprite Y		BL = Sprite X

lodsw					;Sprite W
xchg cx, ax
lodsw					;Sprite H
xchg ah, al 			;AX * 256
or cx, ax				;CH = Sprite H		CL = Sprite W

;============================================================================
;============================================================================
; write offset prep:
;============================================================================
;============================================================================

mov di, [bp + 12]		;DI = video memory write offset

xor ax, ax
mov al, bl

shl ax, 1				;X * 2
add di, ax				;Add X to write offset
			
xchg bh, bl				;Move BH (y) to BL
xor bh, bh
	
shl bx, 1				;+ Y * 16
shl bx, 1
shl bx, 1
shl bx, 1

mov ax, bx
shl bx, 1				;+ Y * 16 * 4
shl bx, 1
add bx, ax				;= Y * 80
		
add di, bx				;add Y * 80 to write offset

and di, [bp + 16]		;Wrap DI with video wrap offset.

;Store row change value to BX

mov bx, 40				;BL = 40
sub bl, cl				;BL = 40 - Width 
shl bx, 1				;BL * 2

mov dx, 80
sub dl, cl				;DX = 80 - Width

;============================================================================
; CLEAR RECTANGLE
;============================================================================

mov ax, [bp + 10]		;AX = character and attribute

push cx					;Check if video memory offset is
mov cx, [bp + 16]		; safe for drawing without wrap check.
sub cx, 4000
cmp di, cx
pop cx
JA loopYwrap
JMP loopYsafe
;============================================================================

loopYwrap:				;Rectangle requires Wrap checking.

push cx					;Push CL=width, CH=height to stack
xor ch, ch				;Height = 0, for X-loop

;-----------------------------------------------------------------------------
loopXwrap:				;		

stosw					;3		Store AX to ES:[DI], DI + 2

and di, [bp + 16]		;		Wrap DI with video wrap offset.

loop loopXwrap
;--------------------------------------------------------------------------

add di, bx				;2		Change write line by BX

and di, [bp + 16]		;		Wrap DI with video wrap offset.

pop cx					;		Pop CL = Width, CH = Height

dec ch					;2		Height - 1					;
XOR ch, 0				;		IF Height = 0 {
JZ backtolist			;		 GOTO backtolist }
JMP loopYwrap			;		else { GOTO loopy }
;============================================================================

backtolist:
JMP newRectangle
						
;============================================================================
loopYsafe:				;No video wrap check.

push cx					;Push CL=width, CH=height to stack
xor ch, ch				;Height = 0, for X-loop

;-----------------------------------------------------------------------------
loopXsafe:				;		
stosw					;3		Store AX to ES:[DI], DI + 2
loop loopXsafe
;--------------------------------------------------------------------------

add di, bx				;2		Change write line by BX

pop cx					;		Pop CL = Width, CH = Height

dec ch					;2		Height - 1					;
CMP ch, 0				;		IF Height = 0 {
JZ backtolist			;		 GOTO backtolist }
JMP loopYsafe			;		else { GOTO loopy }

;--------------------------------------------------------------------------

aClearList ENDP
;================================================================================	
aRectList PROC

	;---------------------------------------------------------------------------
	; Rectangle Copy Routine v. 8.1
	;
	; 40x50 mode drawing. 2 Pixels per byte.
	;
	; Copies a set of rectangles from Tile Buffer to Video memory.
	;
	; Uses sprite lists to copy many rectangles within one call.
	;
	;---------------------------------------------------------------------------
	;
	; Sprite list format:
	; 00: 	Sprite offset		.....	
	; 02: 	X
	; 04: 	Y					.....		y 255 = END OF LIST
	; 06: 	W								w   0 = Skip this sprite
	; 08: 	H					.....		

	;---------------------------------------------------------------------------
	; Parameter stack offsets
	; Order is inverted from qbasic CALL ABSOLUTE parameter order

	;00 bp
	;02 Qbasic return segment
	;04 Qbasic return offset

	;06 Sprite list offset
	;08 Sprite list segment
	;10 Tile buffer offset
	;12 Tile buffer segment
	;14 Tile buffer Wrap
	;16 Video buffer offset
	;18 Video buffer segment
	;20 Video Wrap (CGA = 16384, EGA-> = 32768)
	;22 Tile buffer Window offset

	;----------------------------------------------------------------------------

		push bp				
		mov bp, sp				;Get stack pointer

		mov di, [bp + 16]		;Video buffer offset * 2
		shl di, 1
		mov [bp + 16], di

		mov si, [bp + 06]		;SI = sprite List read offset
		push si					;Push SI for routine logic

	;============================================================================
	; SPRITE LIST READ
	;============================================================================
	newRectangle:

		mov ds, [bp + 08]		;DS = sprite List read segment
		pop si					;Pop SI, sprite list offset from stack

		lodsw					;AX = DS:[SI], [SI] + 2			Skip sprite offset

		lodsw					;Sprite X
		xchg bx, ax
		lodsw					;Sprite Y

		cmp ax, 255				;Has list ended?
		JNZ continuelist		;
	exit:					;----------------------------------------------------
		pop bp					;		Pop BP
		retf 18					;		EXIT
							;----------------------------------------------------

	continuelist:
		xchg ah, al				;AX * 256
		or bx, ax				;BH = Sprite Y		BL = Sprite X

		lodsw					;Sprite W
		xchg cx, ax
		lodsw					;Sprite H
		xchg ah, al 			;AX * 256
		or cx, ax				;CH = Sprite H		CL = Sprite W

	;----------------------------------------------------------------

		push si					;Push Sprite list offset to stack										

		and cl, cl				;IF Width = 0, skip this list entry.
		jz newRectangle

	;============================================================================
	;============================================================================
	; write offset prep:
	;============================================================================
	;============================================================================

		mov di, [bp + 16]		;DI = video memory write offset
		mov si, [bp + 10]		;SI = tile buffer read offset
			
		xor ax, ax
		mov al, bl

		add si, ax				;Add X to read offset

		shl ax, 1				;X * 2
		add di, ax				;Add X to write offset
					
						
		xchg bl, bh				;Move BH (y) to BL
		xor bh, bh
			
		shl bx, 1				;+ Y * 16
		shl bx, 1
		shl bx, 1
		shl bx, 1

		mov ax, bx
		shl bx, 1				;+ Y * 16 * 4
		shl bx, 1
		add bx, ax				;= Y * 80
				
		add di, bx				;add Y * 80 to write offset
		inc di					;DI + 1, to hit the attribute byte.

		and di, [bp + 20]		;		Wrap DI with video wrap offset.

		add si, bx				;add Y * 80 to read offset
		add si, [bp + 22]		;add Window offset to read offset

	;Store row change value to BX

		mov bx, 40				;BL = 40
		sub bl, cl				;BL = 40 - Width 
		shl bx, 1				;BL * 2

		mov dx, 80
		sub dl, cl				;DX = 80 - Width

	;============================================================================
	; COPY RECTANGLE
	;============================================================================

		mov ds, [bp + 12]		;DS = Tile Buffer segment
		mov es, [bp + 18]		;ES = video memory write segment (0xB800)

		push cx					;Check if video memory offset is
		mov cx, [bp + 20]		; safe for drawing without wrap check.
		sub cx, 4000
		cmp di, cx
		pop cx
		JA loopYwrap
		JMP loopYsafe
	;============================================================================

	loopYwrap:				;Rectangle requires Wrap checking.
		
		push cx				;Push CX...
				
		xchg ax, bx			;Move CR-value to AX
		
		mov bx, cx			;Get value for jump.
		xor bh, bh			;Clear height-value from jump.
		
		mov cx, [bp + 20]	;...Store Wrap value to CX.
		
		shl bx, 1
		jmp [rectWrap + bx]
		rectWrap	dw	rwi00, rwi01, rwi02, rwi03, rwi04, rwi05, rwi06, rwi07, rwi08, rwi09, rwi10, rwi11, rwi12, rwi13, rwi14, rwi15, rwi16, rwi17, rwi18, rwi19, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi20, rwi40, rwi40, rwi40, rwi40, rwi40
	;-----------------------------------------------------------------------------
rwi40:
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
		movsb
		inc di
		and di, cx
rwi20:
		movsb
		inc di
		and di, cx
rwi19:
		movsb	
		inc di	
		and di, cx
rwi18:
		movsb	
		inc di	
		and di, cx
rwi17:
		movsb	
		inc di	
		and di, cx
rwi16:
		movsb	
		inc di	
		and di, cx
rwi15:
		movsb	
		inc di	
		and di, cx
rwi14:
		movsb	
		inc di	
		and di, cx
rwi13:
		movsb	
		inc di	
		and di, cx
rwi12:
		movsb	
		inc di	
		and di, cx
rwi11:
		movsb	
		inc di	
		and di, cx
rwi10:
		movsb					;3		Store DS:[SI] to ES:[DI], SI + 1, DI + 1
		inc di					;2		DI + 1
		and di, cx				;		Wrap DI with video wrap offset.
rwi09:
		movsb	
		inc di	
		and di, cx
rwi08:
		movsb	
		inc di	
		and di, cx
rwi07:
		movsb	
		inc di	
		and di, cx
rwi06:
		movsb	
		inc di	
		and di, cx
rwi05:
		movsb	
		inc di	
		and di, cx
rwi04:
		movsb	
		inc di	
		and di, cx
rwi03:
		movsb	
		inc di	
		and di, cx
rwi02:
		movsb	
		inc di	
		and di, cx
rwi01:
		movsb	
		inc di	
		and di, cx
rwi00:
		;--------------------------------------------------------------------------

		add di, ax				;2		Change write line by BX
		add si, dx				;2		Change read line by DX

		and di, [bp + 20]		;		Wrap DI with video wrap offset.

		pop cx					;		Pop CL = Width, CH = Height
		
		dec ch					;2		Height - 1, IF Height = 0 {
		JZ backToRectList		;		 GOTO backtolist } ELSE
		push cx					;		Push CX...
		mov cx, [bp + 20]		; 		and store Wrap value there.
		jmp [rectWrap + bx]		;		Draw next line.
		;============================================================================

	backToRectList:
		JMP newRectangle
							
	;============================================================================
	loopYsafe:				;No video wrap check.
		
		xchg ax, bx			;Move CR-value to AX
		
		mov bx, cx			;Get value for jump.
		xor bh, bh			;Clear height-value from jump.
		
		shl bx, 1	
		jmp [rectSafe + bx]
		rectSafe	dw	ri00, ri01, ri02, ri03, ri04, ri05, ri06, ri07, ri08, ri09, ri10, ri11, ri12, ri13, ri14, ri15, ri16, ri17, ri18, ri19, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri20, ri40, ri40, ri40, ri40, ri40
	;-----------------------------------------------------------------------------
	ri40:
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
		movsb	
		inc di
	ri20:
		movsb					
		inc di					
	ri19:
		movsb	
		inc di
	ri18:
		movsb	
		inc di
	ri17:
		movsb	
		inc di
	ri16:
		movsb	
		inc di
	ri15:
		movsb	
		inc di
	ri14:
		movsb	
		inc di
	ri13:
		movsb	
		inc di
	ri12:
		movsb	
		inc di
	ri11:
		movsb	
		inc di
	ri10:
		movsb					;3		Store DS:[SI] to ES:[DI], SI + 1, DI + 1
		inc di					;2		DI + 1
	ri09:
		movsb	
		inc di
	ri08:
		movsb	
		inc di
	ri07:
		movsb	
		inc di
	ri06:
		movsb	
		inc di
	ri05:
		movsb	
		inc di
	ri04:
		movsb	
		inc di
	ri03:
		movsb	
		inc di
	ri02:
		movsb	
		inc di
	ri01:
		movsb	
		inc di
	ri00:
	;--------------------------------------------------------------------------
				
		add di, ax				;2		Change write line by AX
		add si, dx				;2		Change read line by DX
		
		dec ch					;2		Height - 1, if Zero
		jz safeRectEnd 
		jmp [rectSafe + bx]		;		draw next line, ELSE
safeRectEnd:
		jmp backToRectList		;		GOTO backtolist }

;--------------------------------------------------------------------------
aRectList ENDP
;================================================================================	
aSpriteList PROC

;---------------------------------------------------------------------------
; Sprite drawing routine version 8.1
;
; 40x50 mode drawing. 2 Pixels per byte.
;
; Draws two pixels at once, compositing them to canvas with a mask byte. byte[Pixel | Pixel] byte[Mask | Mask]
;
; Uses a sprite list to draw many sprites before far return.
; Clips sprites at screen edges.
;
;---------------------------------------------------------------------------
;
; Sprite list format:
; 00: 	Sprite offset		.....	
; 02: 	X
; 04: 	Y					.....		y 255 = END OF LIST
; 06: 	W								w   0 = Skip this sprite
; 08: 	H					.....		

;---------------------------------------------------------------------------
; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 bp
;02 Qbasic return segment
;04 Qbasic return offset

;06 Sprite list offset
;08 Sprite list segment
;10 Sprite bank offset
;12 Sprite bank segment
;14 Video offset
;16 Video segment
;18 Screen buffer Wrap (CGA = 16383, EGA-> = 32767)
;----------------------------------------------------------------------------
				push bp				
				mov bp, sp				;Get stack pointer
				
				mov di, [bp + 14]		;Video buffer offset * 2
				shl di, 1
				mov [bp + 14], di
				
				mov si, [bp + 06]		;SI = sprite List read offset
				push si					;Push SI for routine logic

;============================================================================
; SPRITE LIST READ
;============================================================================

newSprite:
				mov ds, [bp + 08]		;DS = sprite List read segment
				pop si					;Pop SI, sprite list offset from stack

				lodsw					;AX = DS:[SI], [SI] + 2			AX = Sprite offset
				push ax					;Store AX to stack														;Push 1

				lodsw					;Sprite X
				mov bx, ax
				lodsw					;Sprite Y

				cmp ax, 255				;Has list ended?
				JNZ continuelist		;
				exit:					;----------------------------------------------------
				pop ax					;		Pop AX															;Pop 1 / Exit
				pop bp					;		Pop BP
				retf 14					;		EXIT
										;----------------------------------------------------

continuelist:
				xchg al, ah				;Swap AL with AH
				or bx, ax				;BH = Sprite Y		BL = Sprite X

				lodsw					;Sprite W
				mov cx, ax
				lodsw					;Sprite H
				xchg al, ah				;Swap AL with AH
				or cx, ax				;CH = Sprite H		CL = Sprite W

				and cl, cl				;IF Width = 0 THEN goto newSprite
				jz newSprite

				add cl, 50				;Width + 50 to avoid clipping overflows with negative values.
				add ch, 50				;Height + 50, ditto.

;----------------------------------------------------------------
				pop di					;Pop Sprite offset from stack (this was pushed as AX)					;Pop 1
				push si					;Push Sprite list offset to stack										
				mov si, di				;Start to read from Sprite Bank

;============================================================================
; WIDTH CLIP TEST
;============================================================================

testXclip_left:							;CLIPTEST
				cmp bl, 20				;Sprite X < 0 Clip Test			X has been offset by 20 to avoid overflows with negative values.
				JL xClip_left			;
				JMP testXclip_right		;
xClip_left:								;Clip = (20 - x)
				mov dl, 20				;
				sub dl, bl				;
				mov dh, 0				;
				add si, dx				;Sprite Offset + Clip
				add si, dx				;Sprite Offset + Clip
				sub cl, dl				;Sprite Width  - Clip
				mov bl, 20				;Sprite X = 0
				JMP testWidthLess

testXclip_right:						;CLIPTEST
				mov dh, bl				;Sprite (X + Width) > 39 Clip Test
				add dh, cl				;
				cmp dh, 109				;Compare to 59 + 50, since Width has been offset by 50 + 20
				JA xClip_right			;
				JMP testWidthLess		;
xClip_right:							;
				sub dh, 110				;Clip = Clip - (40 + 20 + 50)
				sub cl, dh				;Width - Clip


testWidthLess:							;IF Width > 0, skip this sprite.
				cmp cl, 50
				JA testWidthOver
				JMP skipSprite
testWidthOver:
cmp cl, 70
JL widthOK
JMP skipSprite

widthOK:
				sub cl, 50				;Width back to normal value
				sub bl, 20				;X back to normal value

;============================================================================
; HEIGHT CLIP TEST
;============================================================================

testYclip_up:							;CLIPTEST
				cmp bh, 20				;Sprite Y < 0 Clip Test			Y has been offset by 20 to avoid overflows with negative values.
				JL yClip_up				;
				JMP testYclip_down		;
yClip_up:								;Clip = (20 - y)
				mov dl, 20				;
				sub dl, bh				;
				mov dh, 0				;
				sub ch, dl				;Sprite Height - Clip

				shl dx, 1				;Clip = Clip * 80
				shl dx, 1
				shl dx, 1
				shl dx, 1
				
				mov ax, dx
				shl dx, 1
				shl dx, 1
				
				add dx, ax

				add si, dx				;Sprite Offset + Clip
				mov bh, 20				;Sprite Y = 0
				JMP testHeightLess

testYclip_down:							;CLIPTEST
				mov dh, bh				;Sprite (Y + Height) > 50 Clip Test
				add dh, ch				;
				cmp dh, 119				;Compare to 69 + 50, since Height has been offset by 50 + 20
				JA yClip_down			;
				JMP testHeightLess		;
yClip_down:								;
				sub dh, 120				;Clip = Clip - (50 + 20 + 50)
				sub ch, dh				;Height - Clip


testHeightLess:							;IF Height < 0, skip this sprite.
				cmp ch, 50
				JA testHeightOver
				JMP skipSprite
testHeightOver:
cmp ch, 100
JL heightOK
JMP skipSprite

heightOK:
				sub ch, 50				;Height back to normal value
				sub bh, 20				;X back to normal value

;============================================================================
; STORE CLIPPED VALUES BACK TO SPRITE LIST
;============================================================================

				mov es, [bp + 08]		;ES = sprite List segment.
				pop di					;Pop Sprite list offset.
				push di					;Push Sprite list offset back to stack.

				sub di, 8				;Go back a few Words to write our clipped values back.

				xor ax, ax				;AX = 0

				mov al, bl				;AL = X
				stosw					;ES:DI = AX, DI + 2
				mov al, bh				;AL = Y
				stosw					;ES:DI = AX, DI + 2
				mov al, cl				;AL = W
				stosw					;ES:DI = AX, DI + 2
				mov al, ch				;AL = H
				stosw					;ES:DI = AX, DI + 2

;============================================================================
;============================================================================
; write_offset_prep:
;============================================================================
;============================================================================

				mov di, [bp + 14]		;DI = screen buffer write offset
					
				xor ax, ax
				mov al, bl
				shl ax, 1				;X * 2
				add di, ax				;Add X to write offset
								
				xor bl, bl				;Calculate BH (y) * 80, currently at Y * 256
				shr bx, 1				;
				shr bx, 1				;BX / 4
				mov ax, bx				;AX = Y * 64
				shr bx, 1				;BX / 4
				shr bx, 1				;BX = Y * 16
				add bx, ax				;BX = Y * 80
						
				add di, bx				;add Y * 80 from write offset
				inc di					; + 1, to hit the attribute.

				and di, [bp + 18]		;Wrap DI with video wrap offset.

				;Store row change value to BX

				mov bx, 0
				mov bl, 40				;BL = 40
				sub bl, cl				;BL = 40 - Width 
				shl bx, 1				;BL * 2

				mov dx, [bp + 18]		;DX = Video wrap offset

spritedraw:

;============================================================================
; DRAW SPRITE
;============================================================================

				mov ds, [bp + 12]		;DS = Sprite Bank Segment
				mov es, [bp + 16]		;ES = screen buffer write segment

				push dx					;Check if video memory offset is safe for drawing without wrap check.
				sub dx, 1600			;Compare to Video wrap - 1600, which means 20 pixels maximum height for sprites.
				cmp di, dx
				pop dx
				JA loopYwrap
				JMP loopYsafe

;============================================================================

loopYwrap:								;Sprite requires Wrap check.

				push cx					;Push CL=width, CH=height to stack
				xor ch, ch				;Height = 0, for X-loop
				
				push bx
				mov bx, cx
				xor bh, bh
				shl bx, 1	
				jmp [spriteWrap + bx]
spriteWrap	dw	swi00, swi01, swi02, swi03, swi04, swi05, swi06, swi07, swi08, swi09, swi10								
;-----------------------------------------------------------------------------
swi10:

				lodsw					;5		AX = DS:[SI], [SI] + 2
				and ah, es:[di]			;2		AND canvas with mask
				or al, ah				;2		OR canvas with sprite colour
				stosb					;3		Store AL to ES:[DI], DI + 1
				inc di					;2		DI + 1
				and di, dx				;		Wrap DI with video wrap offset.
swi09:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi08:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi07:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi06:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi05:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi04:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi03:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi02:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi01:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi00:
;--------------------------------------------------------------------------
				pop bx
				
				add di, bx				;2		Change write line by BX
				add si, bx				;2		Change read line by BX 

				and di, dx				;		Wrap DI with video wrap offset.

				pop cx					;		Pop CL = Width, CH = Height
				dec ch					;2		Height - 1					;
										;		IF Height = 0 {
				JZ backtolist			;		 GOTO backtolist }
JMP loopYwrap							;		else { GOTO loopy }
;--------------------------------------------------------------------------

backtolist:
JMP newsprite

;--------------------------------------------------------------------------

loopYsafe:							
				mov dx, bx
				mov bx, cx
				xor bh, bh
				shl bx, 1	
				jmp [spriteSafe + bx]
spriteSafe	dw	si00, si01, si02, si03, si04, si05, si06, si07, si08, si09, si10								
si10:
				 lodsw					;5		AX = DS:[SI], [SI] + 2
				 and ah, es:[di]		;2		AND canvas with mask
				 or al, ah				;2		OR canvas with sprite colour
				 stosb					;3		Store AL to ES:[DI], DI + 1
				 inc di					;2		DI + 1
si09:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si08:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si07:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si06:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si05:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si04:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si03:
				 lodsw	
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si02:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si01:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si00:
;----------------------------------------------------------------------------------
				add di, dx				;Change write line by BX
				add si, dx				;Change read line by BX 

				dec ch
				jnz safeContinue
				jmp newSprite
safeContinue:					
				jmp [spriteSafe + bx]
				
;============================================================================
; STORE CLIPPED VALUES BACK TO SPRITE LIST
;============================================================================
skipSprite:				
				mov es, [bp + 08]		;ES = sprite List segment.
				pop di					;Pop Sprite list offset.
				push di					;Push Sprite list offset back to stack.

				sub di, 4				;Go back a few Words to write our clipped values back.

				xor ax, ax				;AX = 0

				stosw					;Width = 0, ES:DI = AX, DI + 2	
jmp newSprite

;============================================================================
aSpriteList ENDP
;================================================================================	
aTileArea PROC

;============================================================================
;
; Tile area drawing routine		v. 8.01
;
; 40x50 mode drawing. 2 Pixels per byte.
;
; Draws a 20x3 tile area from Tile Map to Tile Buffer, using Tile Bank graphics.
;
;============================================================================

; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 bp
;02 Qbasic return segment
;04 Qbasic return offset

;06 tileBank offset
;08 tileMap offset
;10 tileBuffer offset
;12 tileBuffer Segment
;14 Write area offset
;16 Tilemap read offset
;============================================================================

push bp
mov bp,sp

;---------------------------------------------------------------------------

begin:

mov es, [bp + 12]				;ES = tile buffer seg
mov di, [bp + 10]				;DI = tile buffer ofs

mov ds, [bp + 12]				;DS = Tilebuffer seg
mov si, [bp + 16]				;SI = Tilemap ofs
add si, [bp + 08]

mov ax, [bp + 14]				;Set write offset to point to the desired area. (Y * 640)
mov bx, 1920
mul bx

add di, ax

mov bx, 76						;BX = Tile drawing CR value.

;============================================================================
; Loop
;============================================================================

mov dh, 3						;Y Loop = 3
loopY:

mov cx, 20						;Draw 20 tiles per row
loopX:
;============================================================================
; Tile Map, read tile index
;============================================================================
xor ax, ax						;AX = 0
lodsb							;AL = Tile index
shl ax, 1						;AX * 32 to get tile read offset
shl ax, 1
shl ax, 1
shl ax, 1
shl ax, 1

push si							;Push SI (as tile map read offset)

mov si, ax						;SI = Tile bank offset
add si, [bp + 06]				;SI + AX (tile index)

push di							;Store current DI (buffer write offset)
;----------------------------------------------------------------------------
; Tile BLIT
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw

pop di							;pop DI
add di, 4						;DI + 4

pop si							;POP SI
loop loopx
;----------------------------------------------------------------------------
add di, 560
dec dh
cmp dh, 0
je exit
JMP loopY
;----------------------------------------------------------------------------

exit:

pop bp
retf 12

;============================================================================
aTileArea ENDP
;================================================================================	
aTileDraw PROC

;============================================================================
;
; Update Tile routine v. 8.01
;
; 40x50 mode drawing. 2 Pixels per byte.
;
; Draws a single tile in the specified tile Map and Buffer offset.
;
;============================================================================

; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 bp
;02 Qbasic return segment
;04 Qbasic return offset

;06 tileBank offset
;08 tileMap offset
;10 tileBuffer offset
;12 tileBuffer Segment
;14 Tilemap read offset
;16 Tile X offset
;18 Tile Y offset
;
;============================================================================

push bp
mov bp,sp

;---------------------------------------------------------------------------

begin:

mov es, [bp + 12]				;ES = tile buffer seg
mov di, [bp + 10]				;DI = tile buffer ofs

mov ds, [bp + 12]				;DS = Tilebuffer seg
mov si, [bp + 14]				;SI = Tilemap tile read offset
add si, [bp + 08]				;	+ Tilemap start offset

mov ax, [bp + 18]				;DI = DI + (Y * 640)
mov bx, 640
mul bx
add di, ax						

mov ax, [bp + 16]				;DI = DI + (X * 4)
shl ax, 1
shl ax, 1
add di, ax						

;---------------------------------------------------------------------------

xor ax, ax						;AX = 0
lodsb							;AL = Tile index
shl ax, 1						;AX * 32 to get tile read offset
shl ax, 1
shl ax, 1
shl ax, 1
shl ax, 1

mov si, ax						;SI = Tile bank offset
add si, [bp + 06]				;SI + AX (tile index)

mov bx, 76						;BX = Tile draw CR value.

;----------------------------------------------------------------------------
; Tile BLIT
;----------------------------------------------------------------------------
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw
add di, bx
movsw
movsw
;----------------------------------------------------------------------------

exit:

pop bp
retf 14

;================================================================================	
aTileDraw ENDP
;================================================================================	
aTilePan PROC

;============================================================================
;
; Tile buffer Pan routine v8.0
;
; 40x50 mode drawing. 2 Pixels per byte.
;
; Pans tile buffer graphics 24 pixels up or down.
;
;============================================================================

; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 bp
;02 Qbasic return segment
;04 Qbasic return offset

;06 tileBuffer offset
;08 tileBuffer Segment
;10 Pan direction (0 = up, 1 = down)

;============================================================================

push bp
mov bp,sp

;---------------------------------------------------------------------------

begin:

mov es, [bp + 08]				;ES = tile buffer seg
mov di, [bp + 06]				;DI = tile buffer ofs

mov ds, [bp + 08]				;DS = Tilebuffer seg
mov si, [bp + 06]				;SI = Tilemap ofs

mov ax, [bp + 10]

cmp ax, 0
je panup
jmp pandown

;----------------------------------------------------------------------------
; Pan Up
;----------------------------------------------------------------------------
panup:
add si, 1920
mov cx, 2880
rep movsw

jmp exit
;----------------------------------------------------------------------------
; Pan Down
;----------------------------------------------------------------------------
pandown:
add di, 5760
add si, 3840
mov cx, 960
rep movsw

mov di, [bp + 06]				;DI = Tilebuffer ofs
mov si, [bp + 06]				;SI = Tilebuffer ofs

add di, 3840
add si, 1920
mov cx, 960
rep movsw

mov di, [bp + 06]				;DI = Tilebuffer ofs
mov si, [bp + 06]				;SI = Tilebuffer ofs

add di, 1920
mov cx, 960
rep movsw

;----------------------------------------------------------------------------

exit:
pop bp
retf 6

;================================================================================	
aTilePan ENDP
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
;================================================================================	
aPrint PROC

;============================================================================
;
; Print routine 1.0
;
; 40x50 mode drawing. Characters consist of two half-height glyphs.
;
; Uses a 58+58 byte lookup table for glyph-pairs. 
; Each half-height row can be assigned it's own attribute byte.
;
; Can print one line of text up to 40 characters in length.
;
;============================================================================

; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 bp
;02 Qbasic return segment
;04 Qbasic return offset

;06 Write offset
;08 Write segment
;10 Row
;12 Column
;14 Text data offset
;16 Text data segment
;18 Color attributes
;20 Buffer wrap

;============================================================================

	push bp
	mov bp,sp

;---------------------------------------------------------------------------
	mov dx, [bp + 20]				;DX = Buffer wrap
	
	mov es, [bp + 08]				;ES = Write seg
	mov di, [bp + 06]				;DI = Write ofs

	mov ax, [bp + 10]				;AX = Row	Add row and column to write offset
	xchg ah, al						;AX * 256
	shr ax, 1						;AX = Row * 128
	shr ax, 1						;AX = Row * 64
	mov bx, ax						
	shr ax, 1						;AX = Row * 32
	shr ax, 1						;AX = Row * 16
	add ax, bx						;AX = Row * 80
	add ax, [bp + 12]				;AX += Column * 2
	add ax, [bp + 12]
	add di, ax						;Add AX to write offset
	and di, dx						;DI Buffer wrap
		
	mov ds, [bp + 16]				;DS = Text seg
	
	mov bx, [bp + 14]				;BX = Glyph lookup ofs
	
;------------------------------------------------------------------------------	
	push di							;Push DI, current write offset for next row
	
	mov si, [bp + 14]				;SI = Text ofs + 118 to read text string.
	add si, 118						
	
	xor ax, ax						;AX = 0
	
	mov ch, [bp + 18] 				;CH = Color attribute 1
	
printLoop1:							;Print Glpyh row 1 ------------------------
	lodsb							;AL = Character from text
	
	cmp al, 124						;EOL?
	je printPrep2

	push si							;Push Text string read offset
	mov si, bx						;SI = Glyph lookup offset
	
	add si, ax						;Add character - 32 to offset
	sub si, 32						
	
	lodsb							;AL = DS:SI		Get glyph from lookup

	mov ah, ch						;AH = Color attribute
	stosw							;ES:DI 	Print glyph and attribute, DI+2
	and di, dx						;DI Buffer wrap
	xor ax, ax						;AX = 0
	
	pop si							;Pop SI = Text string read offset

jmp printLoop1						;Next character     -----------------------

printPrep2:
	pop di							;Restore DI
	add di, 80						;DI Next row.
	
	mov si, [bp + 14]				;SI = Text ofs + 118 to read text string.
	add si, 118						
	
	add bx, 59						;BX = Glyph 2 lookUp offset
		
	mov ch, [bp + 19] 				;CH = Color attribute 2
	
printLoop2:							;Print Glpyh row 1 ------------------------
	lodsb							;AL = Character from text
	
	cmp al, 124						;EOL?
	je exit

	push si							;Push Text string read offset
	mov si, bx						;SI = Glyph lookup offset
	
	add si, ax						;Add character - 32 to offset
	sub si, 32						
	
	lodsb							;AL = DS:SI		Get glyph from lookup

	mov ah, ch						;AH = Color attribute
	stosw							;ES:DI 	Print glyph and attribute, DI+2
	and di, dx						;DI Buffer wrap
	xor ax, ax						;AX = 0
	
	pop si							;Pop SI = Text string read offset

jmp printLoop2						;Next character     -----------------------

exit:
;------------------------------------------------------------------------------

	pop bp
	retf 16

;================================================================================	
aPrint ENDP
;================================================================================	
aInitVideo PROC

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

;============================================================================

	push bp
	mov bp,sp

;---------------------------------------------------------------------------
	mov es, [bp + 08]				;ES = Write seg (video adapter data)
	mov di, [bp + 06]				;DI = Write ofs (video adapter data)
	
;---------------------------------------------------------------------------
test_VGA:
	xor ax, ax						;Interrupt call 10h AH=1Bh AL=0 BX=0
	mov ah, 01bh					;Get VGA functionality and state.
	xor bx, bx						;dumps 64 bytes at ES:[DI]
	
	int 10h
		
	cmp al, 01bh					;If AL=1Bh this is a VGA
	je detected_VGA
	
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
	jmp detected_VGA

test_EGA:
	xor ax, ax
	xor bx, bx						;Interrupt call 10h:
	mov ah, 12h						;Get EGA information.
	mov bl, 10h						;Returns BL > 4 if not EGA.
									
	int 10h							
									
	cmp bl, 4						;BL =< 4 means we're EGA
	ja test_CGA						;compatible.
	jmp detected_EGA

test_CGA:
	
	int 11h							;Interrupt call: Equipment check.
	
	and ax, 30h						;Check bits 4 & 5
	
	cmp ax, 30h						;If bits are on, this is a mono adapter.
	jz detected_MONO
	jmp detected_CGA

;---------------------------------------------------------------------------
	
detected_MONO:
	mov dx, 0						;Video adapter data for game
	jmp get_current_mode
	
detected_VGA:
	mov dx, 3						;Video adapter data for game
	jmp get_current_mode

detected_EGA:
	mov dx, 2						;Video adapter data for game
	jmp get_current_mode

detected_CGA:
	mov dx, 1						;Video adapter data for game
	jmp get_current_mode

get_current_mode:
	mov ax, 0F00h					;Get current video mode
	int 10h							;Stored to BL
	
	mov es, [bp + 08]				;ES = Write seg (video adapter data)
	mov di, [bp + 06]				;DI = Write ofs (video adapter data)
	add di, 2
	stosw							;AL = current video mode
	
	cmp dx, 1						;If adapter is EGA/VGA
	ja set_EGAVGA					;set mode with interrupts
	jmp exit
	
set_EGAVGA:
	xor ax, ax
	mov al, 01h						;Set video mode 01h
	int 10h							;40x25 colour text mode
	
	mov ax, 1112h					;Load and activate 8x8 character
	xor bx, bx						;set 0
	int 10h
	
	mov ax, 1003h					;Select FG Blink / 16 bg colors (BL = 0)
	mov bx, 0000h					;Clear whole BX to avoid problems on some adapters.
	int 10h
	
;------------------------------------------------------------------------------
exit:
	mov es, [bp + 08]				;ES = Write seg (video adapter data)
	mov di, [bp + 06]				;DI = Write ofs (video adapter data)
	
	mov ax, dx						;Store detected video adapter
	stosw							;data at ES:[DI]
	
	pop bp
	retf 4

;================================================================================	
aInitVideo ENDP
;================================================================================	
aExitVideo PROC
;============================================================================
;
;	Sets video mode back to original state
;
;============================================================================

; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 bp
;02 Qbasic return segment
;04 Qbasic return offset

;06 Old video mode
;08 Video adapter

;============================================================================

	push bp
	mov bp,sp

;---------------------------------------------------------------------------
	
	mov ax, [bp + 06]				;AX = old mode
		
	xor ah, ah						;Clear AH, just in case.
	int 10h							;Set mode

	mov dx, [bp + 08]				;DX = video adapter
	cmp dx, 3						;IF adapter is VGA, set scanlines to 400
	jne exit
	
	mov ax, 1202h
	xor bx, bx
	mov bl, 30h
	int 10h
	
	
;------------------------------------------------------------------------------
exit:
	
	pop bp
	retf 4

;================================================================================	
	
	
aExitVideo ENDP
;================================================================================	
aTileRead PROC
;============================================================================
;
; Read tile from tilemap
;
; Returns a tile index from the tilemap from position (x,y).
;
;============================================================================

; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 bp
;02 Qbasic return segment
;04 Qbasic return offset

;06 tileMap offset
;08 tileBuffer Segment
;10 Tile X
;12 Tile Y
;14 Result return offset
;16 Result return segment

;============================================================================

	push bp
	mov bp,sp

;---------------------------------------------------------------------------

begin:

	mov ds, [bp + 08]				;DS = Tilebuffer seg
	mov si, [bp + 06]				;SI = Tilemap tile read offset

	mov es, [bp + 16]				;ES:DI = Result seg:ofs
	mov di, [bp + 14]

	mov ax, [bp + 10]				;AX = X / 4
	shr ax, 1
	shr ax, 1
	add si, ax						;SI += AX

	mov ax, [bp + 12]				;AX = Y * 8 (from pixel coords to tile index)
	and ax, 0FFF8h					;Remove pixels from coordinate
	mov bx, ax						;BX = AX / 2
	shr bx, 1
	shl ax, 1						;AX = AX * 2
	add ax, bx						;AX = Y * 20
	add si, ax						;SI += AX

	lodsb							;AL = tile index at (x,y)
	xor ah, ah						;AH = 0

	stosw							;Store result at ES:DI

;---------------------------------------------------------------------------

exit:

	pop bp
	retf 12

;================================================================================	
aTileRead ENDP
;================================================================================	
aTileWrite PROC
;============================================================================
;
; Write a tile to tilemap.
;
; Write a tile index into the tilemap at (x,y).
;
;============================================================================

; Parameter stack offsets
; Order is inverted from qbasic CALL ABSOLUTE parameter order

;00 bp
;02 Qbasic return segment
;04 Qbasic return offset

;06 tileMap offset
;08 tileBuffer Segment
;10 Tile X
;12 Tile Y
;14 Tile index to write.

;============================================================================

	push bp
	mov bp,sp

;---------------------------------------------------------------------------

begin:

	mov es, [bp + 08]				;ES = Tilebuffer seg
	mov di, [bp + 06]				;DI = Tilemap tile read offset

	mov ax, [bp + 10]				;AX = X / 4
	shr ax, 1
	shr ax, 1
	add di, ax						;DI += AX

	mov ax, [bp + 12]				;AX = Y * 8 (from pixel coords to tile index)
	and ax, 0FFF8h					;Remove pixels from coordinate
	mov bx, ax						;BX = AX / 2
	shr bx, 1
	shl ax, 1						;AX = AX * 2
	add ax, bx						;AX = Y * 20
	add di, ax						;DI += AX

	mov ax, [bp + 14]				;AX = Tile index to write
	
	stosb							;Write index at ES:DI

;---------------------------------------------------------------------------

exit:

	pop bp
	retf 10

;================================================================================	
aTileWrite ENDP

public aTileWrite

public aTileRead

public aExitVideo

public aInitVideo

public aPrint

public aKBremove

public aTilePan

public aTileDraw

public aTileArea

public aSpriteList

public aRectList

public aClearList

public aKBinit

public aPageFlip

end