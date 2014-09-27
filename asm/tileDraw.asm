.model medium,basic

.code

;================================================================================	
aDrawTile PROC

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
shl ax, 2
add di, ax						

;---------------------------------------------------------------------------

xor ax, ax						;AX = 0
lodsb							;AL = Tile index
shl ax, 5						;AX * 32 to get tile read offset

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
aDrawTile ENDP

public aDrawTile

end
