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
shl ax, 5						;AX * 32 to get tile read offset

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
