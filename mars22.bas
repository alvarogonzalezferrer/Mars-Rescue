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
	
	dx AS SINGLE
	dy AS SINGLE
	
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

CALL drawPlayer

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

	'bounding box
	'LINE (player.x, player.y)-(player.x + 6, player.y + 7), 15, B
END SUB

SUB initGame
	' each new game call this
	
	'player init
	player.x = 160
	player.y = 0
	player.dx = 0
	player.dy = 0
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
	SCREEN , , 0, 1
	CALL createMars
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

