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
