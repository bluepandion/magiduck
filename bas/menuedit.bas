DEFINT A-Z

DIM SHARED mTag(40) AS INTEGER
DIM SHARED mText(40) AS STRING
DIM SHARED mNext(40) AS INTEGER
DIM SHARED mPrev(40) AS INTEGER
DIM SHARED mColor(40) AS INTEGER
DIM SHARED mX(40) AS INTEGER
DIM SHARED mY(40) AS INTEGER
DIM SHARED mW(40) AS INTEGER
DIM SHARED mH(40) AS INTEGER
DIM SHARED mRectW AS INTEGER
DIM SHARED mRectH AS INTEGER
DIM SHARED mRectX AS INTEGER
DIM SHARED mRectY AS INTEGER
DIM SHARED mRectChar AS STRING*1
DIM SHARED mRectColor AS INTEGER
DIM SHARED mFirst AS INTEGER
DIM SHARED mExist AS INTEGER

'$INCLUDE: 'libelf.bi'

elfInit
aKBRemove

main

quit

SUB main
	STATIC sel = 0
	
	DO		
		d$ = INKEY$
		SELECT CASE d$
		CASE chr$(27)
			EXIT SUB
		CASE 		
		END SELECT
		aPageFlip gfx.pageOfs(gfx.page XOR 1), gfx.pageOfs(gfx.page)
		gfx.page = gfx.page XOR 1
	LOOP
END SUB

SUB drawItem(n)	
	IF mExist(n) THEN
		w40char.text = mText(n) + "|"
		aPrint gfx.videoWrap, mColor(n), mX(n), mY(n), &HB800, gfx.pageOfs(gfx.page)
	END IF
END SUB	