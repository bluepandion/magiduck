.model medium,basic

.code

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
			
shr bx, 8				;Move BH (y) to BL 
	
shl bx, 4				;+ Y * 16
mov ax, bx
shl bx, 2				;+ Y * 16 * 4
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

push cx					;Push CL=width, CH=height to stack
mov ch, 0				;Height = 0, for X-loop

;CMP si, [bp + 14]		;	IF (SI => BufferWrap) THEN
;JB noBufferWrap			;	
;	sub si, 7680		;		SI - 7680
;noBufferWrap:			;	END IF
	
;-----------------------------------------------------------------------------
loopXwrap:				;		

movsb					;3		Store DS:[SI] to ES:[DI], SI + 1, DI + 1
inc di					;2		DI + 1

and di, [bp + 20]		;		Wrap DI with video wrap offset.

loop loopXwrap
;--------------------------------------------------------------------------

add di, bx				;2		Change write line by BX
add si, dx				;2		Change read line 

and di, [bp + 20]		;		Wrap DI with video wrap offset.

pop cx					;		Pop CL = Width, CH = Height

dec ch					;2		Height - 1					;
CMP ch, 0				;		IF Height = 0 {
JZ backtolist			;		 GOTO backtolist }
JMP loopYwrap			;		else { GOTO loopy }
;============================================================================

backtolist:
JMP newRectangle
						
;============================================================================
loopYsafe:				;No video wrap check.

push cx					;Push CL=width, CH=height to stack
mov ch, 0				;Height = 0, for X-loop

;-----------------------------------------------------------------------------
loopXsafe:				;		
movsb					;3		Store DS:[SI] to ES:[DI], SI + 1, DI + 1
inc di					;2		DI + 1
loop loopXsafe
;--------------------------------------------------------------------------

add di, bx				;2		Change write line by BX
add si, dx				;2		Change read line 

pop cx					;		Pop CL = Width, CH = Height

dec ch					;2		Height - 1					;
CMP ch, 0				;		IF Height = 0 {
JZ backtolist			;		 GOTO backtolist }
JMP loopYsafe			;		else { GOTO loopy }

;--------------------------------------------------------------------------
aRectList ENDP

public aRectList

end