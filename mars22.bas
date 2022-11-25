DECLARE SUB setupAstronauts ()
DECLARE SUB drawAstronauts ()
DECLARE SUB createMars ()
DECLARE FUNCTION waitForKey$ ()
DECLARE SUB IntroScreen ()
DECLARE SUB MeasureCPU ()
DECLARE SUB initGame ()
DECLARE SUB initLevel ()
DECLARE SUB drawPlayer ()
' ------------------------------------------
' MARS RESCUE
' ------------------------------------------
' GAME FOR "LOST CARTRIDGE" JAM
' BY KRONOMAN - COPYRIGHT (C) 2022
' ------------------------------------------
' SOURCE CODE FOR QBASIC - MSDOS
' ONLY TESTED IN DOSBOX AND QB45
' ------------------------------------------
' Some tech info:
' I use mode 7, that has 16 EGA colors and
' many video pages.
' I use page 2 for background
' I use page 1 for drawing
' I use page 0 to show
' This avoids flicker
' ------------------------------------------

' CPU temporizer
DIM SHARED CPUtempo AS LONG
'temporizer ratio - automatically adjusted
DIM tempoRatio AS LONG
tempoRatio = 500 'default...
DIM looptempo AS LONG
DIM loopfor AS LONG
looptempo = 0

' mars map height map
DIM SHARED mapH(320) AS INTEGER

'colors of mars map
DIM SHARED skyBG
DIM SHARED marsFG
DIM SHARED starsFG

LET skyBG = 0 '  sky
LET marsFG = 4 ' land
LET starsFG = 15 ' stars

' ------------------------------------------
' game types
TYPE PLAYERtype
	x AS SINGLE
	y AS SINGLE
	
	' direction x,y
	dx AS SINGLE
	dy AS SINGLE
	
	spdx AS SINGLE ' speed x
	spdy AS SINGLE ' speed y
	
	mdx AS SINGLE ' max speed x
	mdy AS SINGLE ' max speed y
	
	grav AS SINGLE ' gravity
	
	fuel AS SINGLE

	score AS INTEGER
	life AS INTEGER

	onGround AS INTEGER ' player on ground?
END TYPE

TYPE ASTRONAUTtype
	x AS SINGLE
	y AS SINGLE
	dx AS SINGLE
	dy AS SINGLE
	frame AS INTEGER
	ia AS INTEGER
END TYPE

' characters and stuff
DIM SHARED MAXASTRONAUTS AS INTEGER ' max astronauts on level
DIM SHARED ACTIVEASTRONAUTS AS INTEGER ' active astronauts on this level, when 0, go to next level!
MAXASTRONAUTS = 25
DIM SHARED player AS PLAYERtype
DIM SHARED astronauts(MAXASTRONAUTS) AS ASTRONAUTtype

' ------------------------------------------
' HARDWARE SETUP
SCREEN 0
CLS
COLOR 7, 0
PRINT "Wait please...loading game!"
CALL MeasureCPU
PRINT CPUtempo

' START THE FUN
RANDOMIZE TIMER
SCREEN 7

DO   ' whole game loop

' intro
CALL IntroScreen

CLS
COLOR 15
PRINT "TRAVELING TO MARS SURFACE!"
PRINT "PLEASE WAIT..."

CALL initGame

' game level setup
CALL initLevel

' init astronauts
CALL setupAstronauts

ACTIVEASTRONAUTS = 3 ' DEBUG ADD MORE EACH WAVE!
 
' main game loop of a level
wannaExit = 0 ' wait for ESC key
DO
	'keyboard
	d$ = UCASE$(INKEY$)

	IF d$ = CHR$(27) THEN wannaExit = 1

	' left
	IF d$ = CHR$(0) + "K" OR d$ = "A" THEN player.dx = player.dx - player.spdx
	
	' right
	IF d$ = CHR$(0) + "M" OR d$ = "D" THEN player.dx = player.dx + player.spdx
		
	' up
	IF d$ = CHR$(0) + "H" OR d$ = "W" THEN player.dy = player.dy - player.spdy
	
	' down
	IF d$ = CHR$(0) + "P" OR d$ = "S" THEN player.dy = player.dy + player.spdy
		
	' move
	IF player.onGround = 0 THEN player.x = player.x + player.dx
   
	player.y = player.y + player.dy

	' gravity
	player.dy = player.dy + player.grav

	' constrain
	IF player.dx < -player.mdx THEN player.dx = -player.mdx
	IF player.dx > player.mdx THEN player.dx = player.mdx
	IF player.dy < -player.mdy THEN player.dy = -player.mdy
	IF player.dy > player.mdy THEN player.dy = player.mdy

	IF player.x < 0 THEN
		player.x = 0
		player.dx = 0
	END IF
   
	IF player.x > 314 THEN
		player.x = 314
		player.dx = 0
	END IF

	IF player.y < 0 THEN
		player.y = 0
		player.dy = 0
	END IF

	IF player.y > 193 THEN
		player.y = 193
		player.dx = 0
	END IF

	' friction
	IF player.dx < 0 THEN player.dx = player.dx + player.grav
	IF player.dx > 0 THEN player.dx = player.dx - player.grav
	IF ABS(player.dx) < player.grav THEN player.dx = 0

	'check crash against ground first
	IF mapH(player.x + 3) <= player.y + 7 THEN
	  ' touched ground
	  player.y = mapH(player.x + 3) - 7
	  player.dy = 0
	  player.dx = 0

	  ' debug check crash!
	  player.onGround = 1 'prevent X movement
	ELSE
		player.onGround = 0
	END IF


	' drawing game frame
	' draw off screen 
	SCREEN , , 1, 0
	PCOPY 2, 1' copy background

	' do astronauts
	CALL drawAstronauts

	' do player
	CALL drawPlayer
	
	' HUD
	LOCATE 1, 1
	PRINT USING "> Fuel #### --- Life## --- Score#####"; player.fuel; player.life; player.score
	LOCATE 2, 1
	PRINT USING "> dx##.# dy##.#"; player.dx; player.dy
	IF player.onGround THEN PRINT "* LANDED *"

	' flip page
	PCOPY 1, 0
	SCREEN , , 0, 0
	
	' --- high resolution timer simulation
	' measure time and pause to slow down game frames
	idle2 = TIMER
	IF tempoRatio = 0 THEN tempoRatio = 1 'self adjust
	IF tempoRatio > 5000 THEN tempoRatio = 5000
	looptempo = CPUtempo / tempoRatio
	FOR loopfor = 0 TO looptempo
		'do something to slow down cpu , useless stuff,
		idle = idle + 1
		idle = 0
		idle = loopfor
	NEXT
	  
	 
	IF ABS(TIMER - idle2) < .02 THEN tempoRatio = tempoRatio - 1 ' slow down
	IF ABS(TIMER - idle2) > .02 THEN tempoRatio = tempoRatio + 1 ' speed up


LOOP WHILE wannaExit <> 1

LOOP ' big game loop with menus and all


'---------- end ------------

SUB createMars
' creates mars background
LINE (0, 0)-(320, 200), skyBG, BF

FOR s = 0 TO 100
	x = RND * 320
	y = RND * 200
	PSET (x, y), starsFG
NEXT

LET y = RND * 50 + 100
LET y2 = RND * 50 + 100
LET segment = 8

FOR x = 0 TO 320 STEP segment
  
	LINE (x, y)-(x + segment, y2), marsFG
  
	y = y2
	y2 = y + (RND * 20 - 10)

	IF y2 < 100 THEN y2 = 100
	IF y2 > 198 THEN y2 = 198
  
NEXT
					   
PAINT (0, 199), marsFG


' scan map height
FOR x = 0 TO 320
	FOR y = 99 TO 199
		LET c = POINT(x, y)
		IF c = marsFG THEN
			mapH(x) = y
			EXIT FOR
		END IF
	NEXT
NEXT


END SUB

SUB drawAstronauts
	FOR i = 0 TO ACTIVEASTRONAUTS
		LINE (astronauts(i).x + 2, astronauts(i).y)-(astronauts(i).x + 2, astronauts(i).y + 1), 11
		
		LINE (astronauts(i).x + 2, astronauts(i).y + 2)-(astronauts(i).x + 2, astronauts(i).y + 3), 3
		
		' animate
		IF astronauts(i).frame < 10 THEN
			
			
			' arms
			LINE (astronauts(i).x, astronauts(i).y)-(astronauts(i).x + 1, astronauts(i).y + 1), 3
			
			LINE (astronauts(i).x + 3, astronauts(i).y + 1)-(astronauts(i).x + 4, astronauts(i).y), 3
			
			'legs
			LINE (astronauts(i).x, astronauts(i).y + 5)-(astronauts(i).x + 1, astronauts(i).y + 4), 3
			
			LINE (astronauts(i).x + 3, astronauts(i).y + 4)-(astronauts(i).x + 4, astronauts(i).y + 5), 3
			
		ELSE
			
			' arms
			LINE (astronauts(i).x, astronauts(i).y + 2)-(astronauts(i).x + 1, astronauts(i).y + 1), 3
			
			LINE (astronauts(i).x + 3, astronauts(i).y + 1)-(astronauts(i).x + 4, astronauts(i).y + 2), 3
			
			'legs
			LINE (astronauts(i).x + 1, astronauts(i).y + 5)-(astronauts(i).x + 1, astronauts(i).y + 4), 3
			
			LINE (astronauts(i).x + 3, astronauts(i).y + 4)-(astronauts(i).x + 3, astronauts(i).y + 5), 3
		END IF

		astronauts(i).frame = astronauts(i).frame + 1
		IF astronauts(i).frame > 20 THEN astronauts(i).frame = 0
	NEXT

END SUB

SUB drawPlayer
	LINE (player.x + 1, player.y + 1)-(player.x + 5, player.y + 5), 10, BF
	CIRCLE (player.x + 3, player.y + 3), 3, 2
	LINE (player.x, player.y + 6)-(player.x, player.y + 7), 2
	LINE (player.x + 6, player.y + 6)-(player.x + 6, player.y + 7), 2
	LINE (player.x + 3, player.y + 6)-(player.x + 3, player.y + 7), 2

	'bounding box is x + 6, y  + 7
	'LINE (player.x, player.y)-(player.x + 6, player.y + 7), 15, B

   ' ---------------------
   ' draw fire from boosters
   IF player.dy < 0 THEN
	  FOR i = 0 TO 2 + ABS(INT(player.dy))
		PSET (player.x + RND * 6, player.y + 7 + RND * (3 + ABS(INT(player.dy)))), 14
	  NEXT
   END IF


END SUB

SUB initGame
	' each new game call this
	
	'player init
	player.x = 160
	player.y = 0
	player.dx = 0
	player.dy = 0
	
	player.spdx = .5
	player.spdy = .6
	
	player.grav = .1
	
	player.mdx = 3
	player.mdy = 3
	
	player.fuel = 3000
	player.score = 0
	player.life = 3
END SUB

SUB initLevel
	' each new level call this
	
	' player reset for level
	player.x = 160
	player.y = 0
	player.dx = 0
	player.dy = 0
	player.fuel = 3000
	
	player.onGround = 0 ' not on ground 
   
	' draw mars off screen
	' active page 2,view page 0
	SCREEN , , 2, 0 ' page 2 has the background in cache
	CALL createMars
	PCOPY 2, 1 ' i keep another copy in 1 to double buffer
	PCOPY 1, 0 ' show to screen
	SCREEN , , 0, 0
END SUB

SUB IntroScreen
COLOR 14

CLS

PRINT "MARS RESCUE"
PRINT "==== ======"
PRINT

COLOR 15
PRINT "By ";
COLOR 11
PRINT "KRO";
COLOR 15
PRINT "NO";
COLOR 11
PRINT "MAN";
COLOR 15
PRINT " - (c) 2022"


COLOR 14
PRINT
PRINT
PRINT "THE CREW OF THE FIRST MARS EXPLORER"
PRINT "IS STRANDED ON THE SURFACE"
PRINT
PRINT "THEIR ONLY HOPE IS YOU!"
PRINT "BRING THEM BACK ALIVE!"
PRINT
COLOR 15
PRINT "* HOW TO PLAY *"
PRINT
PRINT "USE ARROW KEYS TO FIRE THRUSTERS"
COLOR 12
PRINT "DO NOT COLLIDE AGAINST MARS SURFACE"
COLOR 10
PRINT "RESCUE ALL THE ASTRONAUTS!"

COLOR 14

PRINT
PRINT

PRINT
PRINT

PRINT "-- PRESS ANY KEY  --"
COLOR 4
PRINT "-- OR ESC TO EXIT --"
COLOR 15

K$ = waitForKey$

IF K$ = CHR$(27) THEN
SCREEN 0
WIDTH 80, 25
COLOR 12
PRINT "THANKS FOR PLAYING!"
		   
COLOR 15
PRINT "By ";
COLOR 11
PRINT "KRO";
COLOR 15
PRINT "NO";
COLOR 11
PRINT "MAN";
COLOR 15
PRINT " - (c) 2022"

END
END IF


END SUB

SUB MeasureCPU

	'measures CPU performance to try to emulate a high resolution timer
	T = TIMER
	CPUtempo = 0
	DO
		CPUtempo = CPUtempo + 1
		IF CPUtempo > 2000000000 THEN CPUtempo = 2000000000 'avoid overflow on fast CPUs
	LOOP UNTIL ABS(TIMER - T) > 1

END SUB

SUB setupAstronauts
	' puts astronauts on map
	FOR i = 0 TO MAXASTRONAUTS
		astronauts(i).x = RND * 310 + 5
		astronauts(i).y = mapH(astronauts(i).x + 2) - 5
		astronauts(i).dx = 0
		astronauts(i).dy = 0
		astronauts(i).frame = RND * 20
		astronauts(i).ia = 0
	NEXT
	ACTIVEASTRONAUTS = MAXASTRONAUTS
END SUB

FUNCTION waitForKey$
DO
	K$ = INKEY$
LOOP UNTIL K$ <> ""

waitForKey$ = K$

END FUNCTION

