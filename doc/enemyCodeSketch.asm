.model tiny

.code

PROC aHandleBehavior
	push es
	push di
	push ds
	push si
	
	mov	 	ds, actorListSeg
	mov 	si, actorListOfs
FN_LOADNEXTACTOR:
	lodsw
	mov		bx, ax
	shl		bx, 1
	jmp		[actorJumpTable+bx]
	actorJumpTable	dw	actor1, actor1, actor1
	jmp exit
;------------------------------------------------------------------------------
@actor1:
	mov		ax, 0
	CALL 	NEAR	FN_LOADACTORDATA
@actor1ret:
	mov		ax, 0	
	CALL	NEAR	FN_TILEREAD
;==============================================================================	
exit:	
	pop si
	pop ds
	pop di
	pop es
	retf 0
;==============================================================================		
aHandleBehavior ENDP

FN_LOADACTORDATA PROC
	mov bx, ax
	lodsw	mov		actorX+bx, ax
	lodsw	mov		actorY+bx, ax
	lodsw	mov		actorVecX+bx, ax
	lodsw	mov		actorVecY+bx, ax
	ret
FN_LOADACTORDATA ENDP

FN_TILEREAD PROC
	ret
FN_TILEREAD ENDP

	C_PLAYEROFFSET			dw	0
	C_ACTORSIZE				dw	0
	
	actorX		dw	0, 0, 0, 0
	actorY		dw	0, 0, 0, 0
	actorParent	dw	0, 0, 0, 0
	actorColl	dw	0, 0, 0, 0
	actorVecX	dw	0, 0, 0, 0
	actorVecY	dw	0, 0, 0, 0
	actorCount	dw	0, 0, 0, 0
	actorJump	dw	0, 0, 0, 0
	actorDir	dw	0, 0, 0, 0
	actorAnim	dw	0, 0, 0, 0
	actorFrame	dw	0, 0, 0, 0
	
	tRead		db	0
	tRead2		db	0
	a			db	0
	b			db	0
	c			db	0
	i			dw	0
	i2			dw	0