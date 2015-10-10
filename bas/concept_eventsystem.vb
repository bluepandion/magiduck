	actor(n).count = actor(n).count + 1
	IF actor(n).count => actor(n).jump THEN
		DO	
		FOR nn = 1 to 3
			actor(n).x = actor(n) + 8
			IF actor(n).x > 72 THEN actor(n).x = 0: actor(n).y = actor(n).y + 8
			c = tileRead(actor(n).x, actor(n).y)		
			actor(n).anim = c
			actor(n).frame = c
			actor(n).frame = CVI(chr$(c) + chr$(actor(n).frame)
		NEXT nn
		SELECT CASE c
		nn = actor(n).dir
		CASE 1
			actor(n).dir = actor(n).frame
		CASE 10
			actor(nn).x = actor(n).frame
		END SELECT
		LOOP WHILE actor(n).anim < 255
		'
		'Set next event time
		actor(n).jump = actor(n).frame
	END IF