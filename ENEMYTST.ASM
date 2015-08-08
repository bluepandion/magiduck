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
	CALL	NEAR	[actorJumpTable+bx]
	actorJumpTable	dw	@actor1
						@actor1
						@actor1
	jmp exit
;==============================================================================	
exit:	
	pop si
	pop ds
	pop di
	pop es
	retf 0
;==============================================================================		
;------------------------------------------------------------------------------
actor1 PROC
	mov		ax, 0
	invoke 	FN_LOADACTORDATA	0
@actor1ret:
	mov		ax, 0
	mov		bx, 0001h
	CALL	NEAR	FN_TILEREAD
	cmp 	tRead, 30
	jb		@actor1_noTCfloor:
	mov		actorVecY, 0
	and		actorY,	&HFFF8
@actor1_noTCfloor:
	
	cmp		actorDir, 5
	jb		
actor1 ENDP
;==============================================================================		
FN_LOADACTORDATA PROC	near	BASIC 
	mov bx, ax
	lodsw	
	mov		actorX+bx, ax
	lodsw	
	mov		actorY+bx, ax
	lodsw	
	mov		actorVecX+bx, ax
	lodsw	
	mov		actorVecY+bx, ax
	ret
FN_LOADACTORDATA ENDP

FN_TILEREAD PROC
	ret
FN_TILEREAD ENDP

FN_DIVIDE PROC
FN_DIVIDE ENDP

FN_MUL PROC
FN_MUL ENDP

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
	gena			db	0
	genb			db	0
	genc			db	0
	geni			dw	0
	geni2			dw	0
	
	cosTable		db	0
	sinTable		db	0
