           MagiDuck
    
	by Toni Svenström 2015   
     mangis.deviantart.com
 www.indiedb.com/games/magiduck
-----------------------------------------------------------------------------

About
	
How to play
	Arrow keys	=	Walk / Look up or down
	Ctrl		=	Jump / Hold to fly
	Alt			=	Shoot
	
	ESC			=	Back in menu / Pause menu
	ENTER		=	Select in menu

Minimum system requirements

	IBM Compatible PC with:
	- Intel 8088, 8086 or higher. 286 recommended.
	- CGA compatible graphics adapter.
	- 256 KB of free memory.
	- 160 KB or free disk space.
	- MS-DOS 3.0 or higher.

DosBox
	
	MagiDuck should run fine in DosBox 0.74 with just about any settings.
	Recommended cycle count: 1000

Programming trivia

	MagiDuck was programmed with QuickBasic 7.1 and x86 Assembly. 
	
	The game uses the 40x25 text mode. The mode is modified to display
	only one half of the characters vertically. This gives it a vertical
	resolution of 50 pseudopixels. 
	
	Each row is filled with ASCII character 222, which fills half a
	character block. This way foreground and background colours can be
	used to address half a pseudopixel, giving the game a horizonal
	resolution of 80 pseudopixels. Unfortunately horizonal scroll
	will move 2 pixels at a time this way.
	
	Actual text will only show top halves of the characters. Because
	of this, all text has to be formed of two rows of these top halves.
	Two top halves of characters are printed on top of each other to
	form something that looks readable enough. This explains the
	strange custom "font" of the game.
	
	CGA does have an 80x25 color text mode, but an original CGA card
	can't handle that mode very well. When handling large amounts of 
	data in video memory, the adapter would not have enough
	bandwidth to handle both the memory operations and sending data
	to the display. This would cause annoying snow-like artifacts on
	the screen. 
	
	Fortunately 40x25 text mode doesn't suffer from this limitation,
	and we can enjoy this game in full colour even on a CGA adapter.
	
	The game engine also features:
		- Smooth scrolling tilemap playfields.
		- Animation system with multi-part sprites.
		- Custom PC-speaker sound routine.
	
License