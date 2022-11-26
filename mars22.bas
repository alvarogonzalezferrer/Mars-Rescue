DECLARE SUB showScore ()
DECLARE SUB showYouWon ()
DECLARE SUB resetPlayer ()
DECLARE SUB drawIntroLevel ()
DECLARE SUB setupAstronauts ()
DECLARE SUB moveAstronauts ()
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

' mars map height map in pixels
DIM SHARED mapH(325) AS INTEGER

'colors of mars map
DIM SHARED skyBG
DIM SHARED marsFG
DIM SHARED starsFG

LET skyBG = 0 '  sky
LET marsFG = 4 ' land
LET starsFG = 7 ' stars

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

    fuel AS INTEGER
    oxygen AS INTEGER

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
    ia AS INTEGER ' not used yet
END TYPE

' characters and stuff
DIM SHARED MAXASTRONAUTS AS INTEGER ' max astronauts on level
DIM SHARED ACTIVEASTRONAUTS AS INTEGER ' active astronauts on this level, when 0, go to next level!

MAXASTRONAUTS = 25 'this is very important, will determine max rescue astronauts and also game lenght

DIM SHARED player AS PLAYERtype
DIM SHARED astronauts(MAXASTRONAUTS) AS ASTRONAUTtype

DIM SHARED currentWave ' current wave

' ------------------------------------------
' HARDWARE SETUP
' START THE FUN
RANDOMIZE TIMER
SCREEN 7

DO ' whole game loop #1

    ' intro
    CALL IntroScreen

    CALL initGame
    currentWave = 0 ' restart game

    DO ' wave loop #2

        currentWave = currentWave + 1

        CLS
        COLOR 15

        ' star field
        FOR s = 0 TO 100
            x = RND * 320
            y = RND * 200
            PSET (x, y), starsFG
        NEXT
        PRINT "MISSION #"; currentWave
        PRINT "TRAVELING TO MARS SURFACE!"
        PRINT USING "MARS CREW ## }"; currentWave + 3
        PRINT
        COLOR 12
        LOCATE 23, 1
        PRINT "PLEASE WAIT...FLYING TO ZONE"
        COLOR 15

        CALL drawIntroLevel

        ' wait
        k$ = INKEY$
        SLEEP 3

        ' -- start level --
        ' game level setup
        CALL initLevel

        ' init astronauts
        CALL setupAstronauts


        ACTIVEASTRONAUTS = 3 + currentWave ' ADD MORE EACH WAVE!
        IF ACTIVEASTRONAUTS > MAXASTRONAUTS THEN
            ACTIVEASTRONAUTS = MAXASTRONAUTS
            'after this wave, you won the game (see below)
        END IF

        ' main game loop of a level #3
        wannaExit = 0 ' wait for ESC key
        DO
            'keyboard
            d$ = UCASE$(INKEY$)

            IF d$ = CHR$(27) THEN wannaExit = 1

            ' left
            IF (d$ = CHR$(0) + "K" OR d$ = "A") AND player.onGround = 0 THEN
                player.dx = player.dx - player.spdx
                player.fuel = player.fuel - 1
            END IF

            ' right
            IF (d$ = CHR$(0) + "M" OR d$ = "D") AND player.onGround = 0 THEN
                player.dx = player.dx + player.spdx
                player.fuel = player.fuel - 1
            END IF

            ' up
            IF d$ = CHR$(0) + "H" OR d$ = "W" THEN
                player.dy = player.dy - player.spdy
                player.fuel = player.fuel - 1
            END IF

            ' down
            IF d$ = CHR$(0) + "P" OR d$ = "S" THEN player.dy = player.dy + player.spdy

            ' move
            ' only if not grounded on x
            IF player.onGround = 0 THEN player.x = player.x + player.dx

            player.y = player.y + player.dy

            ' gravity
            player.dy = player.dy + player.grav

            ' constrain
            IF player.dx < -player.mdx THEN player.dx = -player.mdx
            IF player.dx > player.mdx THEN player.dx = player.mdx
            IF player.dy < -player.mdy THEN player.dy = -player.mdy
            IF player.dy > player.mdy * 2 THEN player.dy = player.mdy * 2 'can fall twice max speed Y

            IF player.x < 0 THEN
                player.x = 0
                player.dx = 0
            END IF

            ' this limit is quite important to not get out of bounds in map height
            IF player.x > 312 THEN
                player.x = 312
                player.dx = 0
            END IF
           
            'dont invade the HUD
            IF player.y < 16 THEN
                player.y = 16
                player.dy = 0
            END IF

            IF player.y > 194 THEN
                player.y = 194
                'player.dy = 0
            END IF

            ' friction
            IF player.dx < 0 THEN player.dx = player.dx + player.grav
            IF player.dx > 0 THEN player.dx = player.dx - player.grav
            IF ABS(player.dx) < player.grav THEN player.dx = 0

            'check crash against ground first
            'CHECK ALL CORNERS AND MIDDLE POINT!
            mapmH = mapH(player.x + 3) 'look highest point below us in left,right,middle
            IF mapH(player.x) < mapmH THEN mapmH = mapH(player.x)
            IF mapH(player.x + 6) < mapmH THEN mapmH = mapH(player.x + 6)

            IF mapmH <= player.y + 7 THEN
                ' touched ground
                player.y = mapmH - 7 'clip to ground to dont go below

                'slide in big slopes
                IF mapH(player.x) > player.y + 10 THEN player.x = player.x - .2
                IF mapH(player.x + 6) > player.y + 10 THEN player.x = player.x + .2

                IF player.onGround = 0 THEN 'touched ground from flying down
                    SOUND 100, 1 ' sound only on first touch down

                    ' check crash!
                    IF player.dy > player.mdy THEN
                        'debug add crash message and sound
                        LOCATE 10, 16
                        COLOR 12
                        PRINT "CRASH!!!"
                        CIRCLE (player.x + 3, player.y + 4), 10, 12
                        PAINT (player.x + 3, player.y + 4), 12
                        'PLAY "MFO3GGFCCo2C"
                        PLAY "mf o1 c c c c p32 d# d d c c b c"
                        player.life = player.life - 1

                        CALL resetPlayer
                    ELSE
                        player.dy = 0
                        player.dx = 0
                        player.onGround = 1 'prevent X movement
                    END IF
                END IF
            ELSE
                player.onGround = 0 ' im flying
            END IF

            'oxygen
            player.oxygen = player.oxygen - 1

            ' drawing game frame
            ' draw off screen
            SCREEN , , 1, 0
            PCOPY 2, 1 ' copy background

            ' astronauts AI AND DRAWING (1 PASS)
            CALL moveAstronauts

            ' do player
            CALL drawPlayer

            ' HUD
            IF player.life > -1 AND player.fuel > 0 AND player.oxygen > 0 THEN
                COLOR 15
                LOCATE 1, 1
                IF player.fuel < 25 OR player.oxygen < 200 THEN COLOR 12 ' low fuel or oxygen
                PRINT USING ">Fuel####|Oxygen####|Score####|Life##"; player.fuel; player.oxygen; player.score; player.life
                LOCATE 2, 1
                COLOR 15
                IF player.onGround THEN
                    COLOR 10
                    PRINT ">* LANDED *<"
                ELSE
                    COLOR 15
                    IF player.dy > player.mdy THEN COLOR 12 'going to crash, warn!
                    IF player.dy > 0 AND player.dy <= player.mdy THEN COLOR 10 'good landing speed
                    PRINT USING ">dx##.# dy##.#<"; player.dx; player.dy
                END IF
            ELSE
                COLOR 12
                LOCATE 1, 1
                PRINT "** MISSION FAILURE **"
                IF player.life < 0 THEN PRINT "> NO SHIPS LEFT"
                IF player.oxygen < 1 THEN PRINT "> NO OXYGEN LEFT"
                IF player.fuel < 1 THEN PRINT "> NO FUEL LEFT"
            END IF

            ' flip page
            PCOPY 1, 0
            SCREEN , , 0, 0
           
            ' -- out of fuel?
            IF player.fuel < 1 THEN
                player.fuel = 0
                LOCATE 10, 14
                COLOR 12
                PRINT "OUT OF FUEL!"
                PLAY "mfo2CCp32def"
                player.life = player.life - 1

                CALL resetPlayer
            END IF

            ' -- out of oxygen
            IF player.oxygen < 1 THEN
                player.oxygen = 0
                LOCATE 10, 13
                COLOR 12
                PRINT "OUT OF OXYGEN!"
                PLAY "mfo2DBDBCCC"
                player.life = player.life - 1

                CALL resetPlayer
            END IF

            ' -- game over --
            ' check if life less than 0 then game over
            IF player.life < 0 THEN
                wannaExit = 1 ' go back to main menu
                LOCATE 11, 12
                COLOR 14
                PRINT "MISSION FAILED!!"
                PLAY "mf o1 c c c c p32 d# d d c c b c p32 d# d# d# d# g f f d# d# d d# "
            END IF



            ' --- high resolution timer simulation
            ' new timer delay system
            ' for QB64 or too fast computers works good too
            ' tested on DOSBOX
            idle = TIMER
            WHILE (ABS(TIMER - idle) < .01)
            WEND
            ' --- end temporizer

            ' loop #3
        LOOP WHILE ACTIVEASTRONAUTS > 0 AND wannaExit <> 1

        IF ACTIVEASTRONAUTS >= MAXASTRONAUTS THEN
            ' won the game!
            CALL showYouWon
            wannaExit = 1 'restart game anyways
        ELSE
            IF wannaExit = 1 THEN CALL showScore ' end game show score
        END IF
        'loop #2
    LOOP WHILE wannaExit <> 1 ' levels loop
 
    'loop #1
LOOP ' big game loop of the game itself


'---------- end ------------

SUB createMars
    ' creates mars background
    LINE (0, 0)-(320, 200), skyBG, BF

    ' star field
    FOR s = 0 TO 100
        x = RND * 320
        y = RND * 200
        PSET (x, y), starsFG
    NEXT

    LET y = RND * 50 + 100
    LET y2 = RND * 50 + 100
    LET segment = 6

    FOR x = 0 TO 320 STEP segment

        LINE (x, y)-(x + segment, y2), marsFG

        y = y2
        y2 = y + (RND * 40 - 20)

        IF y2 < 100 THEN y2 = 100
        IF y2 > 198 THEN y2 = 198

    NEXT

    ' fill
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
    'bugfix for when the player is too near right border
    mapH(320) = 200
    mapH(321) = 200
    mapH(322) = 200
    mapH(323) = 200
    mapH(324) = 200
    mapH(325) = 200
END SUB

SUB drawIntroLevel
    ' intro screen to a new level
    ' mars planet
    CIRCLE (160, 100), 80, 4
    PAINT (160, 100), 4
    ' mars dots
    FOR s = 0 TO 100
        x = RND * 100 - 50
        y = RND * 100 - 50
        x = 160 - x
        y = 100 - y
        PSET (x, y), 6
        IF RND * 50 > 40 THEN CIRCLE (x, y), 1, 6
        IF RND * 50 > 48 THEN CIRCLE (x, y), 2, 6
    NEXT
    ' mars crew location on planet
    x = RND * 100 - 50
    y = RND * 100 - 50
    x = 160 - x
    y = 100 - y
    LINE (x - 3, y - 3)-(x + 3, y + 3), 15, B
    LINE (108, 20)-(x, y - 3), 15

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
    player.spdx = .5
    player.spdy = .6
    
    player.grav = .1
    
    player.mdx = 3
    player.mdy = 3
   
    player.score = 0
    player.life = 3
    'all other vars are set in resetPlayer
END SUB

SUB initLevel
   
    ' draw mars off screen
    ' active page 2,view page 0
    SCREEN , , 2, 0 ' page 2 has the background in cache
    CALL createMars
    PCOPY 2, 1 ' i keep another copy in 1 to double buffer
    PCOPY 1, 0 ' show to screen
    SCREEN , , 0, 0

    ' each new level call this
    CALL resetPlayer
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
    PRINT "THE CREW OF THE FIRST MARS BASE"
    PRINT "IS STRANDED ON THE SURFACE"
    PRINT
    PRINT "THEIR ONLY HOPE IS YOU!"
    PRINT "BRING THEM BACK ALIVE!"
    PRINT
    COLOR 15
    PRINT "* HOW TO PLAY *"
    PRINT
    PRINT "USE ARROW KEYS TO FIRE THRUSTERS"
    PRINT "YOU CAN ALSO USE A,S,D,W"
    COLOR 12
    PRINT "DO NOT COLLIDE AGAINST MARS SURFACE"
    COLOR 10
    PRINT "RESCUE ALL THE ASTRONAUTS!"

    COLOR 14

    PRINT
    PRINT
    PRINT

    PLAY "mbl8o2ccdeffa"

    PRINT "-- PRESS ANY KEY  --"
    COLOR 4
    PRINT "-- OR ESC TO EXIT --"
    COLOR 15

    k$ = waitForKey$

    IF k$ = CHR$(27) THEN
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

SUB moveAstronauts
    'NOTE: i also moved the draw code here to sped up loops
    'only 1 pass over astronaut list, instead of 2

    ' IA of astronauts

    FOR i = 0 TO ACTIVEASTRONAUTS - 1

        ' constraint to screen
        IF astronauts(i).x < 5 THEN
            astronauts(i).x = 5
            astronauts(i).dx = -astronauts(i).dx
        END IF

        IF astronauts(i).x > 315 THEN
            astronauts(i).x = 315
            astronauts(i).dx = -astronauts(i).dx
        END IF

        IF astronauts(i).y < 0 THEN astronauts(i).y = 0
        IF astronauts(i).y > 194 THEN astronauts(i).y = 194

        ' first fall to ground
        IF astronauts(i).y + 6 < mapH(astronauts(i).x + 2) THEN
            astronauts(i).dy = 1.5
        ELSE
            astronauts(i).dy = 0 ' on ground
            astronauts(i).y = mapH(astronauts(i).x + 2) - 6
        END IF
        
        astronauts(i).y = astronauts(i).y + astronauts(i).dy
        astronauts(i).x = astronauts(i).x + astronauts(i).dx
        
        ' CHECK IF THEY COLLIDED WITH PLAYER, AND REMOVE FROM LIST!
        IF ABS(player.x - astronauts(i).x) < 6 AND ABS(player.y - astronauts(i).y) < 10 THEN
        
            PLAY "MBO2L8CFG" ' pickup sound
            
            player.score = player.score + 1
           
            'GIVE MORE FUEL and OXIGEN!
            player.oxygen = player.oxygen + RND * 10 + 5
            player.fuel = player.fuel + RND * 5 + 5

            IF ACTIVEASTRONAUTS > 0 THEN ' remove from list
                astronauts(i) = astronauts(ACTIVEASTRONAUTS - 1)
                ACTIVEASTRONAUTS = ACTIVEASTRONAUTS - 1
                EXIT SUB
            ELSE
                ' WAVE ENDED  , is handled elsewhere
                EXIT SUB
            END IF
        END IF

        ' will try to chase player if near, or wait
        IF astronauts(i).ia < 1 THEN ' only if im not running already
            IF ABS(player.x - astronauts(i).x) < 50 AND ABS(player.y - astronauts(i).y) < 30 THEN
                astronauts(i).dx = 0 'this prevents flicker if we are just below the player ship
                IF player.x < astronauts(i).x + 2 THEN astronauts(i).dx = -.3
                
                IF player.x + 6 > astronauts(i).x + 2 THEN astronauts(i).dx = .3
            ELSE
                astronauts(i).dx = 0 ' wait

                IF RND * 100 < 5 THEN ' tired of waiting, move
                    astronauts(i).dx = ((RND * 200) - 100) / 100
                    astronauts(i).ia = RND * 15 + 5
                END IF
            END IF
        ELSE
            astronauts(i).ia = astronauts(i).ia - 1
        END IF
        

        '-- draw code for astronaut (i)
        LINE (astronauts(i).x + 2, astronauts(i).y)-(astronauts(i).x + 2, astronauts(i).y + 3), 11
       
        'LINE (astronauts(i).x + 2, astronauts(i).y + 2)-(astronauts(i).x + 2, astronauts(i).y + 3), 3
       
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

        astronauts(i).frame = astronauts(i).frame + RND * 3
        IF astronauts(i).frame > 20 THEN astronauts(i).frame = 0

    NEXT
END SUB

SUB resetPlayer
    ' player reset for level OR LIFE LOSS
    player.x = 160
    player.y = 0
    player.dx = 0
    player.dy = 0
    player.fuel = 250
    player.oxygen = 1500
    player.onGround = 0 ' not on ground



    ' GET READY MESSAGE
    ' SHOWN WHEN STARTING LEVEL OR LOSING A LIFE
    LOCATE 10, 15
    COLOR 10
    PRINT "GET READY!"
    idle = TIMER
    WHILE ABS(TIMER - idle) < 1
    WEND
END SUB

SUB setupAstronauts
    ' puts astronauts on map
    FOR i = 0 TO MAXASTRONAUTS
        astronauts(i).x = RND * 310 + 5
        astronauts(i).y = mapH(astronauts(i).x + 2) - 6
        astronauts(i).dx = 0
        astronauts(i).dy = 0
        astronauts(i).frame = RND * 20
        astronauts(i).ia = 0
    NEXT
    ACTIVEASTRONAUTS = MAXASTRONAUTS
END SUB

SUB showScore
    ' shows final score, but only when the mission is a failure
    ' not for winning the game


END SUB

SUB showYouWon
    ' show when you won the game

END SUB

FUNCTION waitForKey$
    DO
        k$ = INKEY$
    LOOP UNTIL k$ <> ""

    waitForKey$ = k$

END FUNCTION

