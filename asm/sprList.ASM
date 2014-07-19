.model medium,basic

.code

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
jmp begin
jumpTable dw safe_00, safe_00, safe_00, safe_00, safe_00, safe_00, safe_00, safe_00, safe_00
;----------------------------------------------------------------------------
begin:
				push bp				
				mov bp, sp				;Get stack pointer

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

				shl dx, 4				;Clip = Clip * 80
				mov ax, dx
				shl dx, 2
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

				and di, [bp + 18]		;		Wrap DI with video wrap offset.

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

;-----------------------------------------------------------------------------
loopXwrap:				;		

				lodsw					;5		AX = DS:[SI], [SI] + 2
				and ah, es:[di]			;2		AND canvas with mask
				or al, ah				;2		OR canvas with sprite colour
				stosb					;3		Store AL to ES:[DI], DI + 1
				inc di					;2		DI + 1

				and di, dx				;		Wrap DI with video wrap offset.

loop loopXwrap
;--------------------------------------------------------------------------

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
				xchg bx, dx
				mov bx, cx
				xor bh, bh
				shl bx, 1
				mov bx, 2
				;jmp [jumpTable + bx]
				jmp newSprite
				jmp [jumpTable]
								
safe_08:
				; lodsw					;5		AX = DS:[SI], [SI] + 2
				; and ah, es:[di]			;2		AND canvas with mask
				; or al, ah				;2		OR canvas with sprite colour
				; stosb					;3		Store AL to ES:[DI], DI + 1
				; inc di					;2		DI + 1
safe_07:
				; lodsw					
				; and ah, es:[di]			
				; or al, ah				
				; stosb					
				; inc di					
safe_06:
safe_05:
safe_04:
safe_03:
safe_02:
safe_01:
safe_00:
;----------------------------------------------------------------------------------
				jmp newSprite
				add di, dx				;Change write line by BX
				add si, dx				;Change read line by BX 

				dec ch
				jnz safeContinue
				jmp newSprite
safeContinue:					
				jmp loopYsafe
;jmp [jumpTable + bx]
				
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

public aSpriteList

end