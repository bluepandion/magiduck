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
			
shr bx, 8				;Move BH (y) to BL 
	
shl bx, 4				;+ Y * 16
mov ax, bx
shl bx, 2				;+ Y * 16 * 4
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
