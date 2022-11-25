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

'colors
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
	
	mdx as single ' max speed x
	mdy as single ' max speed y
	
	grav as SINGLE ' gravity
	
	fuel AS SINGLE

	score AS INTEGER
	life AS INTEGER
END TYPE

' game stuff
DIM SHARED player AS PLAYERtype


' ------------------------------------------
' HARDWARE SETUP
SCREEN 0
CLS
COLOR 7, 0
PRINT "Wait please..."
CALL MeasureCPU
PRINT CPUtempo

' START THE FUN
RANDOMIZE TIMER
SCREEN 7

' intro
CALL IntroScreen

CALL initGame

' game level setup
CALL initLevel

' main game loop of a level
DO
	'keyboard 
	D$ = UCASE$(INKEY$)

	' left
	IF D$ = CHR$(0) + "K" OR D$ = "A" THEN player.dx = player.dx - player.spdx 
	
	' right
	IF D$ = CHR$(0) + "M" OR D$ = "D" THEN player.dx = player.dx + player.spdx
		
	' up
	IF D$ = CHR$(0) + "H" OR D$ = "A" THEN player.dy = player.dy - player.spdy
	
	' down
	IF D$ = CHR$(0) + "P" OR D$ = "D" THEN player.dy = player.dy + player.spdy
		
	

	' move
	player.x = player.x + player.dx 
	player.y = player.y + player.dy 

	' gravity
	player.dy = player.dy + player.grav

	' constrain
	if player.dx < -player.mdx then player.dx = -player.mdx
	if player.dx > player.mdx then player.dx = player.mdx
	if player.dy < -player.mdy then player.dy = -player.mdy
	if player.dy > player.mdy then player.dy = player.mdy

	if player.x < 0 then player.x = 0
	if player.x > 314 then player.x = 314

	if player.y < 0 then player.y = 0
	if player.y > 193 then player.y = 193

	' draw off screen	
	SCREEN , , 1 , 0
	PCOPY 2,1 ' copy background
	CALL drawPlayer

	' flip page
	PCOPY 1, 0
	SCREEN , , 0,0
	
	' --- high resolution timer simulation
	' measure time and pause to slow down game frames
	'Pausar
	idle2 = TIMER
	IF tempoRatio = 0 THEN tempoRatio = 1 'self adjust
	IF tempoRatio > 5000 THEN tempoRatio = 5000
	looptempo = CPUtempo / tempoRatio
	FOR loopfor = 0 TO looptempo
		'Hacer algo aqui, sino funciona muy rapido
		idle = idle + 1
		idle = 0
		idle = loopfor
	NEXT
	  
	 
	IF ABS(TIMER - idle2) < .02 THEN tempoRatio = tempoRatio - 1 ' slow down
	IF ABS(TIMER - idle2) > .02 THEN tempoRatio = tempoRatio + 1 ' speed up


LOOP


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

END SUB

SUB drawPlayer
	LINE (player.x + 1, player.y + 1)-(player.x + 5, player.y + 5), 10, BF
	CIRCLE (player.x + 3, player.y + 3), 3, 2
	LINE (player.x, player.y + 6)-(player.x, player.y + 7), 2
	LINE (player.x + 6, player.y + 6)-(player.x + 6, player.y + 7), 2
	LINE (player.x + 3, player.y + 6)-(player.x + 3, player.y + 7), 2

	'bounding box is x + 6, y  + 7
	'LINE (player.x, player.y)-(player.x + 6, player.y + 7), 15, B
END SUB

SUB initGame
	' each new game call this
	
	'player init
	player.x = 160
	player.y = 0
	player.dx = 0
	player.dy = 0
	
	player.spdx = 0.5
	player.spdy = 0.5
	
	player.grav = 0.2
	
	player.mdx = 3
	player.mdy = 5
	
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
	
	
	' draw mars off screen
	' active page 2,view page 0 
	SCREEN , , 2, 0 ' page 2 has the background in cache
 	CALL createMars
	pcopy 2, 1 ' i keep another copy in 1 to double buffer
	pcopy 1, 0 ' show to screen 
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
COLOR 14

PRINT
PRINT

PRINT
PRINT
PRINT
PRINT "-- PRESS ANY KEY --"

k$ = waitForKey$



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

FUNCTION waitForKey$
DO
	k$ = INKEY$
LOOP UNTIL k$ <> ""

waitForKey$ = k$

END FUNCTION

