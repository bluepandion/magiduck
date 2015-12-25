.model medium,basic

.data

.code

;==============================================================================	
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
	;============================================================================
		push bp	
		mov bp,sp				
		push es
		push di
		push ds
		push si
	;---------------------------------------------------------------------------
	; Copy HUD to set page
	;---------------------------------------------------------------------------
		mov dx, vWrap					;DX = Video wrap offset

		mov ds, hudBuffer
		mov si, hudBuffer+2

		mov es, vSegment				;ES:DI = Page offset * 2
		mov di, [bp + 06]
		shl di, 1
	xor	ch, ch
	mov cl, gfxHudRows					;Hud string = 160 bytes
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
	
		;lodsb							;CL = Video adapter (stored at the
		cmp vAdapter, 5
		ja egaVgaPageflip
	
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
		mov dx, 03DAh					;Check for both Display enable- and
		mov ah, 9						;Vertical retrace bits (0 and 3).

	egavgawait1:						;If currently in retrace, wait
		in  al, dx						;until that's finished.
		and al, ah
	jnz egavgawait1								

	;---------------------------------------------------------------------------
	; EGA/VGA Change page offset
	; This needs to be done before vertical retrace begins with these adapters.
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
		mov dx, vWrap					;DX = Video wrap offset
		mov di, [bp + 08]				;DI = previous page offset * 2
		shl di, 1
		mov ax, 00DEh					;Clear attribute = 00, character = 222
	xor ch, ch
	mov cl, gfxHudRows
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
	;---------------------------------------------------------------------------
	exit:
		pop si
		pop ds
		pop di
		pop es
		pop bp
		retf 4
	;============================================================================
aPageFlip endp
aKBinit PROC
	;------------------------------------------------------------------------------
	;
	; Keyboard init
	;
	;------------------------------------------------------------------------------
	push bp
	push ds

	mov bp, sp

	mov ah, 035h		;Subfunction 35h: Get current interrupt handler address
	mov al, 09h			;Subfunction 09h: Get address of int 09h (keyboard handler)
	int 021h			;Call interrupt 21h
	
	mov kbOld, es
	mov kbOld + 2, bx
	
	mov dx, cs ;[bp + 10]		;DS = New keyboard routine segment
	mov ds, dx			
	mov dx, keyboardInterrupt	;[bp + 8]	;DX = New keyboard routine offset

						;Pass DS:DX to the interrupt
	mov ah, 025h		;Subfunction 25h: Change interrupt handler address
	mov al, 09h			;Subfunction 09h
	int 021h

	pop ds
	pop bp

	retf 
aKBinit ENDP
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

;06 Character and attribute to clear with
;08 Sprite list offset
;10 Sprite list segment
;12 Video offset

;----------------------------------------------------------------------------
push bp			
mov bp, sp				;Get stack pointer
push es
push di
push ds
push si

mov di, [bp + 12]		;Video buffer offset * 2
shl di, 1
mov [bp + 12], di

mov es, vSegment		;ES = memory write segment (video memory or custom)

mov ds, [bp + 10]		;DS = sprite List segment
mov si, [bp + 08]		;SI = sprite List offset

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
pop si
pop ds
pop di
pop es					
pop bp
retf 8					;		EXIT
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

xor bl, bl				;bx = Y * 256
shr bx, 1				
shr bx, 1			
mov ax, bx				;ax = Y * 64
shr bx, 1				
shr bx, 1				;bx = Y * 16
add bx, ax				;bx = Y * 80
		
add di, bx				;add Y * 80 to write offset

and di, vWrap			;Wrap DI with video wrap offset.

;Store row change value to BX

mov bx, 40				;BL = 40
sub bl, cl				;BL = 40 - Width 
shl bx, 1				;BL * 2

mov dx, 80
sub dl, cl				;DX = 80 - Width

;============================================================================
; CLEAR RECTANGLE
;============================================================================

mov ax, [bp + 06]		;AX = character and attribute

push cx					;Check if video memory offset is
mov cx, vWrap			; safe for drawing without wrap check.
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

stosw					;Store AX to ES:[DI], DI + 2

and di, vWrap			;Wrap DI with video wrap offset.

loop loopXwrap
;--------------------------------------------------------------------------

add di, bx				;Change write line by BX

and di, vWrap			;Wrap DI with video wrap offset.

pop cx					;Pop CL = Width, CH = Height

dec ch					;Height - 1					
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
aRectList PROC
	;---------------------------------------------------------------------------
	; Rectangle list Copy
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
	;10 Page offset	
	;12 Tile buffer Window offset	
	;----------------------------------------------------------------------------
		push bp			
		mov bp, sp				
		push es
		push di
		push ds
		push si
		
		mov di, [bp + 10]		;Page offset * 2
		shl di, 1
		mov [bp + 10], di

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
	exit:						;----------------------------------------------------
		pop si
		pop ds
		pop di
		pop es
		pop bp
		retf 8					;		EXIT
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

		mov di, [bp + 10]		;DI = video memory write offset
		mov si, gfxTileBuffer		;SI = tile buffer read offset
			
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

		and di, vWrap		;		Wrap DI with video wrap offset.

		add si, bx				;add Y * 80 to read offset
		add si, [bp + 12]		;add Window offset to read offset

	;Store row change value to BX

		mov bx, 40				;BL = 40
		sub bl, cl				;BL = 40 - Width 
		shl bx, 1				;BL * 2

		mov dx, 80
		sub dl, cl				;DX = 80 - Width

	;============================================================================
	; COPY RECTANGLE
	;============================================================================

		mov ds, gfxTileSeg		;DS = Tile Buffer segment
		mov es, vSegment		;ES = video memory write segment (0xB800)

		push cx					;Check if video memory offset is
		mov cx, vWrap			;safe for drawing without wrap check.
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
		
		mov cx, vWrap		;...Store Wrap value to CX.
		
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

		and di, vWrap		;		Wrap DI with video wrap offset.

		pop cx					;		Pop CL = Width, CH = Height
		
		dec ch					;2		Height - 1, IF Height = 0 {
		JZ backToRectList		;		 GOTO backtolist } ELSE
		push cx					;		Push CX...
		mov cx, vWrap		; 		and store Wrap value there.
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
aSpriteList PROC
;---------------------------------------------------------------------------
; Sprite CLIP & BLIT routine version 8.1
;
; 40x50 mode drawing. 2 Pixels per byte.
;
; Draws two pixels at once, compositing them to canvas with a mask byte.
; byte[Pixel | Pixel] byte[Mask | Mask]
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
;-10 Sprite bank offset
;-12 Sprite bank segment
;-14 Video offset
;-16 Video segment
;18 Screen buffer Wrap (CGA = 16383, EGA-> = 32767)

;06 Sprite list offset
;08 Sprite list segment
;10 Page offset
;----------------------------------------------------------------------------
	push bp
	mov bp, sp				;Get stack pointer
	
	push es
	push di
	push ds
	push si
	
	mov di, [bp + 10]		;Video buffer offset * 2
	shl di, 1
	mov [bp + 10], di
	
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
	pop ax					;		
	pop si
	pop ds
	pop di
	pop es
	pop bp
	retf 6					;		EXIT
							;----------------------------------------------------
continuelist:
	mov bh, al				;BH = Sprite Y		BL = Sprite X
	lodsw					;Sprite W
	mov cx, ax
	lodsw					;Sprite H
	mov ch, al				;CH = Sprite H		CL = Sprite W
	and cl, cl				;IF Width = 0 THEN goto newSprite
	jz newSprite
;----------------------------------------------------------------
	pop di					;Pop Sprite offset from stack (this was pushed as AX)					;Pop 1
	push si					;Push Sprite list offset to stack										
	mov si, di				;Start to read from Sprite Bank
;============================================================================
; WIDTH CLIP TEST
;============================================================================
testXclip_left:				;CLIPTEST
	cmp bl, 0				;Sprite X < 0 Clip Test			X has been offset by 20 to avoid overflows with negative values.
	JL xClip_left			;
	JMP testXclip_right		;
xClip_left:					;Clip = (20 - x)
	mov dh, 255
	mov dl, bl
	sub si, dx				;Sprite Offset + Clip
	sub si, dx				;Sprite Offset + Clip
	add cl, bl				;Sprite Width  - Clip
	xor bl, bl				;X = 0, after clip
	JMP testWidthLess
testXclip_right:			;CLIPTEST
	xor dx, dx
	mov dl, bl				;Sprite (X + Width) > 40 Clip Test
	add dl, cl				;
	cmp dl, 41
	JL testWidthLess
xClip_right:							;
	sub dl, 40				;Clip = (X + Width) - 40
	sub cl, dl				;Width - Clip
testWidthLess:				;IF Width < 0, skip this sprite.
	cmp cl, 0
	JG testWidthOver
	JMP skipSprite
testWidthOver:
cmp cl, 17
JL widthOK
JMP skipSprite
widthOK:
;============================================================================
; HEIGHT CLIP TEST
;============================================================================
testYclip_up:				;CLIPTEST
	cmp bh, 0				;Sprite Y < 0 Clip Test
	JL yClip_up				;
	JMP testYclip_down		;
yClip_up:								
	add ch, bh				;Sprite Height - Clip
	;inc ch
	dec	bh
	mov dx, 0FF00h			;Sprite offset + (Clip * 80)	
	sub dh, bh
	shr dx, 1
	shr dx, 1
	mov ax, dx
	shr dx, 1
	shr dx, 1
	add dx, ax

	add si, dx				;Sprite Offset + Clip
	xor bh, bh				;Sprite Y = 0
	JMP testHeightLess
testYclip_down:				;CLIPTEST
	mov dh, bh				;Sprite (Y + Height) > 51 Clip Test
	add dh, ch				;
	cmp dh, 51				;
	JL testHeightLess
yClip_down:								;
	sub dh, 50				;Clip = (Y + Height) - 50
	sub ch, dh				;Height - Clip
testHeightLess:				;IF Height < 0, skip this sprite.
	cmp ch, 0
	JG testHeightOver
	JMP skipSprite
testHeightOver:
	cmp ch, 24
JL heightOK
JMP skipSprite
heightOK:
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
	mov di, [bp + 10]		;DI = screen buffer write offset
		
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

	;Store row change value to BX
	xor bx, bx
	mov bl, 40				;BL = 40
	sub bl, cl				;BL = 40 - Width 
	shl bx, 1				;BL * 2

	mov dx, vWrap		;DX = Video wrap offset
	and di, dx				;Wrap DI with video wrap offset.
spritedraw:
;============================================================================
; DRAW SPRITE
;============================================================================
	mov ds, gfxSpriteBank	;DS = Sprite Bank Segment
	mov es, vSegment		;ES = screen buffer write segment

	push dx					;Check if video memory offset is safe for drawing without wrap check.
	sub dx, 1600			;Compare to Video wrap - 1600, which means 20 pixels maximum height for sprites.
	cmp di, dx
	pop dx
	JA loopYwrap
	JMP loopYsafe
;============================================================================
loopYwrap:					;Sprite requires Wrap check.

	push cx					;Push CL=width, CH=height to stack
	xor ch, ch				;Height = 0, for X-loop
	
	push bx
	mov bx, cx
	xor bh, bh
	shl bx, 1	
	jmp [spriteWrap + bx]
spriteWrap	dw	swi00, swi01, swi02, swi03, swi04, swi05, swi06, swi07, swi08, swi09, swi10, swi11, swi12, swi13, swi14, swi15, swi16, swi16, swi16
;-----------------------------------------------------------------------------
swi16:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi15:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi14:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi13:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi12:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
swi11:
				lodsw				
				and ah, es:[di]		
				or al, ah			
				stosb				
				inc di				
				and di, dx			
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
JMP loopYwrap				;		else { GOTO loopy }
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
spriteSafe	dw	si00, si01, si02, si03, si04, si05, si06, si07, si08, si09, si10, si11, si12, si13, si14, si15, si16, si16, si16
si16:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si15:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si14:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si13:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si12:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
si11:
				 lodsw					
				 and ah, es:[di]			
				 or al, ah				
				 stosb					
				 inc di					
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
aTileArea PROC
;============================================================================
;
; Tile area drawing routine
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

;06 Write area offset
;08 Tilemap read offset

;============================================================================
push bp
mov bp,sp
push es
push di
push ds
push si
;---------------------------------------------------------------------------

begin:

mov es, gfxTileSeg				;ES = tile buffer seg
mov di, gfxTileBuffer			;DI = tile buffer ofs

mov ds, gfxTileSeg				;DS = Tilebuffer seg
mov si, [bp + 08]				;SI = Tilemap ofs
add si, gfxTileMap

mov ax, [bp + 06]				;Set write offset to point to the desired area. (Y * 640)
and ax, 3						; only accept values 0-3.
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
add si, gfxTileBank				;SI + AX (tile index)

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
pop si
pop ds
pop di
pop es
pop bp
retf 4

;============================================================================
aTileArea ENDP
aTileDraw PROC
;============================================================================
;
; Update Tile 
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

;06 Tilemap read offset
;08 Tile X offset
;10 Tile Y offset

;============================================================================
push bp
mov bp,sp
push es
push di
push ds
push si
;---------------------------------------------------------------------------

begin:

mov es, gfxTileSeg				;ES = tile buffer seg
mov di, gfxTileBuffer			;DI = tile buffer ofs

mov ds, gfxTileSeg				;DS = Tilebuffer seg
mov si, [bp + 06]				;SI = Tilemap tile read offset
add si, gfxTileMap				;	+ Tilemap start offset

mov ax, [bp + 10]				;DI = DI + (Y * 640)
mov bx, 640
mul bx
add di, ax						

mov ax, [bp + 08]				;DI = DI + (X * 4)
shl ax, 1
shl ax, 1
add di, ax						

cmp	di, gfxTileBuffer			;Is the draw offset inside the buffer?
jae	offsetInAbove
jmp	exit
offsetInAbove:
mov ax, 7121
add	ax, gfxTileBuffer
cmp	di, ax
jb	offsetInBelow
jmp	exit
offsetInBelow:

;---------------------------------------------------------------------------

xor ax, ax						;AX = 0
lodsb							;AL = Tile index
shl ax, 1						;AX * 32 to get tile read offset
shl ax, 1
shl ax, 1
shl ax, 1
shl ax, 1

mov si, ax						;SI = Tile bank offset
add si, gfxTileBank				;SI + AX (tile index)

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
pop si
pop ds
pop di
pop es
pop bp
retf 6
;================================================================================	
aTileDraw ENDP
aTilePan PROC
;============================================================================
;
; Tile buffer Pan routine
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

;06 Pan direction (0 = up, 1 = down)

;============================================================================
push bp
mov bp,sp
push es
push di
push ds
push si
;---------------------------------------------------------------------------

begin:

mov es, gfxTileSeg				;ES = tile buffer seg
mov di, gfxTileBuffer			;DI = tile buffer ofs

mov ds, gfxTileSeg				;DS = Tilebuffer seg
mov si, gfxTileBuffer			;SI = Tilemap ofs

mov ax, [bp + 06]

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

mov di, gfxTileBuffer			;DI = Tilebuffer ofs
mov si, gfxTileBuffer			;SI = Tilebuffer ofs

add di, 3840
add si, 1920
mov cx, 960
rep movsw

mov di, gfxTileBuffer			;DI = Tilebuffer ofs
mov si, gfxTileBuffer			;SI = Tilebuffer ofs

add di, 1920
mov cx, 960
rep movsw

;----------------------------------------------------------------------------

exit:
pop si
pop ds
pop di
pop es
pop bp
retf 2
;================================================================================	
aTilePan ENDP
aKBremove PROC
;------------------------------------------------------------------------------
; Keyboard routine remove and restore.
;------------------------------------------------------------------------------
push ds

mov dx, kbOld
mov ds, dx
mov dx, kbOld + 2

mov ah, 025h
mov al, 09h
int 021h

pop ds
retf 0
;------------------------------------------------------------------------------
aKBremove ENDP
aPrint PROC
;============================================================================
;
; Print routine 
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
;14 Color attributes
;16 Buffer wrap
;============================================================================
	push bp
	mov bp,sp
	push es
	push di
	push ds
	push si
;---------------------------------------------------------------------------
	mov dx, [bp + 16]				;DX = Buffer wrap
	
	mov es, [bp + 08]				;ES = Write seg
	mov di, [bp + 06]				;DI = Write ofs
	
	mov ax, es
	cmp ax, 0b800h					;IF screen segment, multiply write ofs by two
	jne notVideoSeg
	shl di, 1
notVideoSeg:
	
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
		
	mov ds, w40char					;DS = Text data seg
	
	mov bx, w40char+2				;BX = Text data ofs (Glyph lookup)
	
;------------------------------------------------------------------------------	
	push di							;Push DI, current write offset for next row
	
	mov si, bx						;SI = Text ofs + 118 to read text string.
	add si, 118						
	
	xor ax, ax						;AX = 0
	
	mov ch, [bp + 15] 				;CH = Color attribute 1
	
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
	
	mov si, bx						;SI = Text ofs + 118 to read text string.
	add si, 118						
	
	add bx, 59						;BX = Glyph 2 lookUp offset
		
	mov ch, [bp + 14] 				;CH = Color attribute 2
	
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

	pop si
	pop ds
	pop di
	pop es
	pop bp
	retf 12

;================================================================================	
aPrint ENDP
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
; 6	ega
; 5 mcga
; 4	tandy 1000
; 3 tandy SLTL
; 2 pcjr
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
	cmp  bl, 0Ah       				;MCGA returns 0x0A..0x0C 
	jb   VGAdetected
	cmp  bl, 0Ch
	jg   VGAdetected
	mov  dx, 5
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
	
	mov dx, 6						;Save adapter # to DX
	jmp exit
;---------------------------------------------------------------------------
test_PCJR:
	mov ax, 0FFFFh
	mov es, ax
	mov di, 000Eh     				;second to last byte PCjr/Tandy BIOS info area
	mov al, 0FDh      				;ends up 0xFD only on the Jr.
	cmp es:[di], al
	jne test_TANDY
	mov dx, 2						;Save adapter # to DX
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
	jnc detected_tandySLTL    		;early Tandy's leave the carry bit set, TL/SL does not
	mov dx, 4						;Save adapter # to DX (tandy 1000)
	jmp exit
detected_tandySLTL:					
	mov dx, 3						;Save adapter # to DX (tandy SLTL)
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
aVideoSet PROC
;============================================================================
;
; Sets video mode to 40x50 tweaked text mode with 8x8 characters
; for desired video adapter:
; 7	vga
; 6	ega
; 5 mcga
; 4	tandy 1000
; 3 tandy SLTL
; 2 pcjr
; 1	cga
; 0 mono
;
; Qbasic stack parameters:
;
; 00 bp
; 02 Qbasic return segment
; 04 Qbasic return offset
;
; 06 Video adapter to set up for.
;
;============================================================================
	push bp
	mov bp, sp
	
	push es
	push di
	push ds
	push si
	
	mov bx, [bp + 6]				;DX = desired video-adapter for setup

	cmp bx, 8
	jb parameterOk					;Only accept unsigned values < 4.
	jmp exit
parameterOk:
	
	mov vAdapter, bl
	mov dx, bx
	
	shl bx, 1
	jmp [jumpTableAdapter + bx]
						
	jumpTableAdapter	dw	exit	;mono
	dw set_CGA						;CGA
	dw set_CGA						;PCJR
	dw set_CGA						;Tandy SLTL
	dw set_CGA						;Tandy 1000
	dw set_CGA_32k					;MCGA
	dw set_EGA						;EGA
	dw set_VGA						;VGA
;-----------------------------------------------------------------------------	
set_VGA:
	mov si, vVGAregs
	mov vWrap, 32767
	jmp set_EGAVGA

set_EGA:
	mov si, vEGAregs
	mov vWrap, 32767
	jmp set_EGAVGA

set_CGA_32k:
	mov si, vCGAregs
	mov vWrap, 32767
	jmp set_EGAVGA
	
set_CGA:
	mov si, vCGAregs
	mov vWrap, 16383
	jmp startLoopVideoRegs

;-----------------------------------------------------------------------------		
set_EGAVGA:
	xor ax, ax
	mov al, 01h						;Set video mode 01h
	int 10h							;40x25 colour text mode
	
	mov ax, 1112h					;Load and activate 8x8 character
	xor bx, bx						;set 0
	int 10h
;-----------------------------------------------------------------------------	
startLoopVideoRegs:	
	mov cx, cs:[si]					;Load # of register data entries for looper.
	inc si
	inc si
	
	cli								;Disable interrupts and NMI (non-maskable interrupts),
	in   al, 70h					; so they don't disrupt CRTC setup.
	and  al, 7Fh					
	out  70h, al

	mov  dx, 03D8h					;Disable video until CRTC setup is done,
	mov  al, 00h					; to protect the monitor hardware.
	out  dx, al

loopRegs:							;Loop through CRTC register data
	mov dx, cs:[si]
	inc si
	inc si
	mov ax, cs:[si]
	inc si
	inc si
	out dx, al
loop loopRegs
;-----------------------------------------------------------------------------	
	mov  dx, 03D8h					;Enable video, 40x25 mode
	xor  ax, ax
	mov  al, 08h
	out  dx, al
	
	in   al, 70h					;Restore interrupts and NMI
	or   al, 80h
	out  70h, al
	sti
	
	cmp vAdapter, 5					;Disable blink for EGA/VGA/MCGA
	jb tweakPCJR
	mov ax, 1003h					;Select FG Blink / 16 bg colors (BL = 0)
	xor bx, bx						;Clear whole BX to avoid problems on some adapters.
	int 10h
	mov ax, 1003h					;Disable blink for VGA one extra time to be sure.
	xor bx, bx						
	int 10h

tweakPCJR:
	cmp vAdapter, 2					;PCJR Blink disable
	jne clearScreen
	mov  dx, 3DAh
	in   al, dx  					;reset flip-flop
	mov  al, 03h 					;mode control 2 
	out  dx, al
	xor  al, al  					;bit 1 off == no blink! 
	out  dx, al	
	
clearScreen:		
	mov es, vSegment				;Clear video-memory with attribute 11
	mov di, 0						; and ASCII 222
	mov ax, 11DEh
	mov cx, vWrap
	inc cx
	shr cx, 1
	rep	stosw
;-----------------------------------------------------------------------------	
exit:	
	pop si
	pop ds
	pop di
	pop es
	
	pop bp
	retf 2
	
vCGAregs:
	dw	21	;Number of entries
	dw 03D4h, 0000h, 03D5h, 0038h
	dw 03D4h, 0001h, 03D5h, 0028h
	dw 03D4h, 0002h, 03D5h, 002Dh
	dw 03D4h, 0003h, 03D5h, 000Ah
	dw 03D4h, 0004h, 03D5h, 003Fh
	dw 03D4h, 0005h, 03D5h, 0006h
	dw 03D4h, 0006h, 03D5h, 0032h
	dw 03D4h, 0007h, 03D5h, 0038h
	dw 03D4h, 0008h, 03D5h, 0002h
	dw 03D4h, 0009h, 03D5h, 0003h
	dw 03D8h, 0008h
vEGAregs:
	dw	36	;Number of entries	
	dw 03D4h, 0000h, 03D5h, 0037h ;H total
	dw 03D4h, 0001h, 03D5h, 0027h ;H Disp end
	dw 03D4h, 0002h, 03D5h, 002Dh ;Start H blank
	dw 03D4h, 0003h, 03D5h, 0037h ;End H Blank
	dw 03D4h, 0004h, 03D5h, 0031h ;Start H Retr
	dw 03D4h, 0005h, 03D5h, 0015h ;End H Retr
	dw 03D4h, 0006h, 03D5h, 0004h ;V Total
	dw 03D4h, 0007h, 03D5h, 0011h ;Overflow
	dw 03D4h, 0008h, 03D5h, 0000h ;Preset row SC
	dw 03D4h, 0009h, 03D5h, 0003h ;Max scanline
	dw 03D4h, 0010h, 03D5h, 00E1h ;V Retr Start
	dw 03D4h, 0011h, 03D5h, 0024h ;V Retr End
	dw 03D4h, 0012h, 03D5h, 00C7h ;V Disp End
	dw 03D4h, 0013h, 03D5h, 0014h ;Offset
	dw 03D4h, 0015h, 03D5h, 00E0h ;Start V blank
	dw 03D4h, 0016h, 03D5h, 00F0h ;End V blank
	dw 03D4h, 0017h, 03D5h, 00A3h ;Mode control
	dw 03D4h, 0018h, 03D5h, 00FFh ;Line compare	
vVGAregs:
	dw	2	;Number of entries
	dw 03D4h, 0009h, 03D5h, 0083h 
;============================================================================
aVideoSet ENDP
aVideoExit PROC
;============================================================================
;
;	Sets video mode back to original state
;
;============================================================================
	mov al, vOldMode				;AX = old mode		
	xor ah, ah						;
	int 10h							;Set mode
	
	cmp vAdapter, 3					;IF adapter is VGA, set scanlines to 400
	jne exit
	
	mov ax, 1202h
	xor bx, bx
	mov bl, 30h
	int 10h
exit:
	retf 0
;================================================================================
aVideoExit ENDP
aSoundNote PROC
	push bp
	mov bp, sp
	
	xor bx, bx
	mov bl, soundPos
	mov ax, [bp + 6]
	mov soundQueue[bx], al
	
	pop bp
	retf 2
aSoundNote ENDP
aSoundFX PROC
	push bp
	mov bp, sp
	push ds
	push si
	
	xor ax, ax
	mov bx, [bp + 8]
	add bl, soundPos
	and bx, 63
	
	mov ds, soundData
	mov si, soundData + 2
	add si, [bp + 6]
	
	fillQueue:
	lodsb
	cmp al, 95
	ja exit
	mov soundQueue[bx], al
	inc bx
	and bx, 63
	jmp fillQueue
	
	exit:
	pop si
	pop ds
	pop bp
	retf 4	
aSoundFX ENDP
aSoundPlay PROC
	xor bx, bx
	mov bl, soundPos
	xor ax, ax
	mov al, soundQueue[bx]
	mov soundQueue[bx], 0
	cmp	ax, 1
	jb	noPlay
	
	shl ax, 1
	xchg bx, ax
	mov ax, freqData[bx]
	xchg al, bl
	cli
	mov al,	0B6h
	out	43h, al
	xchg al, bl
	out 42h, al
	xchg al, ah
	out 42h, al
	mov al, soundOn
	out 61h, al
	sti
	jmp exitPlay
	
	noPlay:	
	cli
	mov al, soundOff
	out 61h, al
	sti
	
	exitPlay:
	inc soundPos
	and soundPos, 63
	
	retf
aSoundPlay ENDP
aSoundStop PROC
	cli
	mov al, soundOff
	out 61h, al
	sti
	mov bx, 0
	clearQueue:
	mov soundQueue[bx], 0
	inc bx
	cmp bx, 64
	jl clearQueue
	retf 0
aSoundStop ENDP
aSetup PROC
	push bp
	mov bp, sp
	push es
	push ds
	push si
	
	mov soundPos, 0			; Setup pc speaker control values.
	in al, 61h
	or al, 3
	mov soundOn, al
	xor al, 3
	mov soundOff, al	
	
	mov ds, [bp + 8]		; Load external data addresses from
	mov si, [bp + 6]		; provided string.
	
	lodsw
	mov soundData, ax
	lodsw
	mov soundData+2, ax
	
	lodsw					
	mov kbArray, ax
	lodsw
	mov kbArray+2, ax	
	
	lodsw
	mov gfxSpriteBank, ax
	lodsw
	mov gfxSpriteBank+2, ax
	
	lodsw
	mov gfxTileSeg, ax
	lodsw
	mov gfxTileBank, ax
	lodsw
	mov gfxTileMap, ax
	lodsw
	mov gfxTileBuffer, ax
	
	lodsw
	mov hudBuffer, ax
	lodsw
	mov hudBuffer+2, ax
	
	lodsw
	mov w40char, ax
	lodsw
	mov w40char+2, ax
	
	pop si
	pop ds
	pop es
	pop bp
	retf 4
aSetup ENDP
aCopyPage PROC
	push bp
	mov bp,sp
	push es
	push di
	push ds
	push si
		
	mov es, vSegment
	mov di, [bp + 6]
	mov ds, vSegment
	mov si, [bp + 8]
	shl di, 1
	shl si, 1
	add di, 160
	add si, 160
	
	mov dx, vWrap
	mov cx, 1920
copyPage:
	and di, dx
	and si, dx
	movsw
loop copyPage
	
	pop si
	pop ds
	pop di
	pop es
	pop bp
	retf 4
aCopyPage ENDP
aMenuHiLite PROC
	; Stack parameters:
	;	06 page offset
	;	08 attribute
	;	10 h
	;	12 w
	;	14 y
	;	16 x
	push bp
	mov bp,sp
	push es
	push di
	push ds
	push si
	
	mov es, vSegment
	mov di, [bp + 6]
	mov ds, vSegment		
	mov si, [bp + 6]
	shl di, 1
	shl si, 1
							; Write offset =
	mov dx, [bp + 14]		; y * 80
	xchg dl, dh
	shr dx, 1
	shr dx, 1
	mov cx, dx
	shr dx, 1
	shr dx, 1
	add dx, cx
	add di, dx
	add di, [bp + 16]		; + x * 2
	add di, [bp + 16]
	add di, 1
	mov si, di
	
	mov dx, 80				; CR value
	mov ax, [bp + 12]
	sub dx, ax
	sub dx, ax
	
	mov bx, [bp + 08]
	
	xor cx, cx
	mov ch, [bp + 10]
loopY:
	push cx
	xor ch, ch
	mov cl, [bp + 12]
	push dx
	mov dx, vWrap
loopX:
	and di, dx
	and si, dx
	lodsw
	mov al, bl
	stosw
loop loopX
	pop dx
	add di, dx
	add si, dx
	xchg bh, bl
	pop cx
	dec ch
jnz loopY

	pop si
	pop ds
	pop di
	pop es
	pop bp
	retf 12
aMenuHiLite ENDP
aUnpackLevelRLE PROC
;
;	Unpack a level tilemap from a string.
;
;	Parameters
;	06	RLE string offset
;	08	RLE string segment
;
	push	bp
	mov 	bp, sp
	push	es
	push	di
	push	ds
	push	si
		
	mov		ds, [bp + 08]
	mov		si, [bp + 06]		
		
	mov		es, gfxTileSeg
	mov		di, gfxTileMap	
	
	xor		dx, dx
	mov		bl, 127				;Bit 8 flags an RLE run	
	mov		cx, 1600
loopRLE:
	lodsb
	cmp		al, bl
	ja		decodeRun
	stosb
	loop	loopRLE
	jmp		exit
decodeRun:	
	and		al, bl
	push	cx
	mov		cl, al
	mov		dl, al
	xor		ch, ch
	lodsb
	rep		stosb
	pop		cx
	sub		cx, dx
	cmp		cx, 1
	jle		exit
	jmp		loopRLE

exit:			
	pop		si
	pop		ds
	pop		di
	pop		es
	pop		bp
	retf	4	
aUnpackLevelRLE ENDP
aUnpackTileGfxRLE PROC
;	Unpack RL encoded tile graphics from a string to tile bank.
;
;	Parameters
;	06	RLE string offset
;	08	RLE string segment
;
	push	bp
	mov 	bp, sp
	push	es
	push	di
	push	ds
	push	si
		
	mov		ds, [bp + 08]
	mov		si, [bp + 06]
	mov		es, gfxTileSeg
	mov		di, gfxTileBank
		
	xor		dx, dx
	xor		ax, ax
	mov		dl, 2
	mov		bl, 0F0h				;RLE run start value
	mov		cx, 1920
loopRLE:
	lodsb	
	cmp		al, bl
	jae		decodeRun
	stosb
	loop	loopRLE
	jmp		exit
decodeRun:
	and		al, 0Fh
	add		al, dl
	sub		cx, ax
	push	cx
	mov		cl, al	
	xor		ch, ch
	lodsb	
	rep		stosb
	pop		cx	
	cmp		cx, 1
	jle		exit
	jmp		loopRLE

exit:	
	pop		si
	pop		ds
	pop		di
	pop		es
	pop		bp
	retf	4		
aUnpackTileGfxRLE ENDP
aUnpackSprites PROC
;
;	Unpack sprites with binary format mask data.
;   Packed data is expected in Tile Buffer.
;
;	Parameters
;	06	Sprite unpack target offset (Aux or Main sprite-bank)
;
	push	bp
	mov 	bp, sp
	push	es
	push	di
	push	ds
	push	si
	
	mov		es,	gfxSpriteBank	
	mov		ds, gfxTileSeg
	mov		si, gfxTileBuffer
	
	mov		di, [bp + 06]			; Copy color data,		
	lodsw							; length provided in first word.
	mov		cx, ax
	shr		cx, 1
copyColor:
	movsb
	inc		di
	movsb
	inc		di
	loop	copyColor
	
	mov		di, [bp + 06]			; Then unpack mask data bits
	inc		di
	mov		bx, 0FF0h
	lodsw
	mov		cx, ax
nextMaskByte:
	lodsb	
	mov		dl, al
	mov		dh, 1
decodeBit0:
	xor		al, al
	test	dl, dh
	jz		decodeBit1
	or		al, bh
decodeBit1:		
	shl		dh, 1
	test	dl, dh
	jz		decodeBit2
	or		al, bl
decodeBit2:
	stosb
	inc		di
	shl		dh, 1

	xor		al, al
	test	dl, dh
	jz		decodeBit3
	or		al, bh
decodeBit3:		
	shl		dh, 1
	test	dl, dh
	jz		decodeBit4
	or		al, bl
decodeBit4:
	stosb
	inc		di
	shl		dh, 1

	xor		al, al
	test	dl, dh
	jz		decodeBit5
	or		al, bh
decodeBit5:		
	shl		dh, 1
	test	dl, dh
	jz		decodeBit6
	or		al, bl
decodeBit6:
	stosb
	inc		di
	shl		dh, 1

	xor		al, al
	test	dl, dh
	jz		decodeBit7
	or		al, bh
decodeBit7:		
	shl		dh, 1
	test	dl, dh
	jz		nextMask
	or		al, bl
nextMask:
	stosb
	inc		di	
	loop 	nextMaskByte
exit:	
	pop		si
	pop		ds
	pop		di
	pop		es
	pop		bp
	retf	2
aUnpackSprites ENDP

;==============================================================================
;
; Timer ISR by DeathShadow / Jason M. Knight
;
; Interface Procedures
;
;==============================================================================
aTimerStart PROC
					
		cmp   timerActive, 00h
		ja    done
		inc   timerActive
		mov   timerTick, 0000h
		mov   timerCount, 08h
		push  ax
		push  bx
		push  dx
		push  ds
		push  es
		push  di
		mov   ax, 3508h
		int   21h
		
		mov   oldTimerISR, bx
		mov   oldTimerISR + 2, es
		
		cli
		mov   ax, cs
		mov   ds, ax
		mov   dx, timerISR
		mov   ax, 2508h
		int   21h
		mov   al, 34h
		out   43h, al
		mov   ax, 2000h
		out   40h, al
		mov   al, ah
		out   40h, al
		sti
			
		pop   di
		pop   es
		pop   ds
		pop   dx
		pop   bx
		pop   ax
		
	done:
		retf 0
		
aTimerStart ENDP
aTimerEnd PROC
		cmp   timerActive, 00h
		je    done
		push  ax
		push  dx
		push  ds
		cli
		mov   al, 34h
		out   43h, al
		xor   al, al
		out   40h, al
		out   40h, al
		mov   timerActive, al
		
		mov   dx, oldTimerISR + 2
		mov   ds, dx
		mov   dx, oldTimerISR
		
		mov   ax, 2508h
		int   21h
		sti
		pop   ds
		pop   dx
		pop   ax
	done:
		retf 0
aTimerEnd ENDP
aTimerWait PROC
		push ax
		mov ax, 6
				
	timerWait:		
		cmp   	timerTick, ax
		jb    	timerWait
		
		mov timerTick, 0000h
		
		pop ax
		
		retf 0
aTimerWait ENDP
aTimerReset PROC
		mov   timerTick, 0000h
		retf 0
aTimerReset ENDP

;==============================================================================
;
; Timer ISR
;
;==============================================================================	

timerISR:
		inc 	timerTick
		dec   	timerCount
		jz    	callOldTimerISR
		push  	ax
		mov   	al, 20h
		out   	20h, al
		pop   	ax
		iret
		
	callOldTimerISR:
		mov  timerCount, 8
		db 234
		oldTimerISR dw 1234h, 5678h, 0000h, 0000h

;==============================================================================
;
; Keyboard interrupt
;
; Modified after Jim Leonard's help & article to ensure it works
; on PC/XT machines.
;
;==============================================================================	
keyboardInterrupt:
	push ds
	push ax
	push bx

	mov bx, kbArray							;Set keyboard array seg/ofs
	mov ds, bx								; to save key states into.
											;
	mov bx, kbArray + 2						;
											
	xor ah, ah								;AH = 0
	in al, 060h								;AL = Read from port 60

	CMP al, 7Fh								;Is it < 127?
	JA keyrelease
		shl al, 1							;AL * 2
		add bx, ax							;BX = Keyboard array offset + Key offset (AX)
		mov al, 1							;Write a one to enable key
		mov ds:[bx], al
		JMP keyExit							;-------------------------------------------
	keyrelease:
											;Key released.
		and al, 7Fh							;Remove higheset bit of AL
		shl al, 1							;AL * 2
		cmp al, 88							;Special handling for player jump toggle
		je keyJump		
		add bx, ax							;BX = Keyboard array offset + Key offset (AX)
		xor al, al
		mov ds:[bx], al						;Write a zero to disable key			
		jmp keyExit
	keyJump:		
		add bx, ax							;BX = Keyboard array offset + Key offset (AX)
		xor al, al
		mov ds:[bx], al						;Write a zero to disable key			
		add bx, 170
		mov al, 1
		mov ds:[bx], al
		
	keyExit:

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

;==============================================================================
;
; Code segment variables
;
;==============================================================================	
		dummyData1		dw	0000h, 0000h
		timerTick 		dw 	0
		timerSecond		dw	0
		timerCount 		db 	8
		timerActive 	db 	0
		
		soundOff		db	0
		soundOn			db	0
		soundPos		db	0

		soundData		dw	0000h, 0000h
							
		soundQueue		db	'0000000000000000000000000000000000000000000000000000000000000000'
		
		freqData		dw	256, 256, 256, 62798, 56818, 54235, 51877, 49715, 45891, 44191, 41144, 38489
		dw 36156 , 34090 , 32248 , 30594 , 29101 , 27117 , 25938 , 24350 , 22945 , 21694 , 20572 , 19244
		dw 18356 , 17292 , 16344 , 15297 , 14550 , 13714 , 12969 , 12175 , 11472 , 10847 , 10286 , 9700
		dw 9108 , 8584 , 8116 , 7697 , 7231 , 6818 , 6449 , 6087 , 5736 , 5423 , 5120 , 4870
		dw 4554 , 4307 , 4058 , 3836 , 3615 , 3418 , 3224 , 3043 , 2875 , 2711 , 2560 , 2415
		dw 2281 , 2153 , 2032 , 1918 , 1810 , 1709 , 1612 , 1521 , 1435 , 1355 , 1280 , 1207
		dw 1140 , 1075 , 1015 , 959 , 898 , 854 , 806 , 760 , 718 , 677 , 639 , 604
		dw 570 , 538 , 507 , 479 , 452 , 427 , 403 , 380 , 359 , 338 , 319 , 301, 256, 256
				
		tileRead		dw	0000h, 0000h
		gfxSpriteBank	dw	0000h, 0000h
		gfxTileSeg		dw	0000h
		gfxTileBank		dw	0000h
		gfxTileMap		dw	0000h
		gfxTileBuffer	dw	0000h		
		hudBuffer		dw	0000h, 0000h
		w40char			dw	0000h, 0000h
		kbArray			dw 	0000h, 0000h
		kbOld			dw  0000h, 0000h
		kbFlags			db	00h
		gfxHudRows		db	10h
		
		vSegment		dw	0b800h
		vOldMode		db	0
		vAdapter		db	0
		vWrap			dw	0000h
									
		dummyData2		dw	0000h, 0000h, 0000h, 0000h
;==============================================================================	

public aPageFlip
public aKBinit
public aClearList
public aRectList
public aSpriteList
public aTileArea
public aTileDraw
public aTilePan
public aKBremove
public aPrint
public aVideoDetect
public aVideoSet
public aVideoExit
public aSoundNote
public aSoundFX
public aSoundPlay
public aSoundStop
public aSetup
public aCopyPage
public aMenuHiLite
public aUnpackLevelRLE
public aUnpackTileGfxRLE
public aUnpackSprites
public aTimerStart
public aTimerEnd
public aTimerWait
public aTimerReset

end