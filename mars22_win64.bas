DECLARE SUB showDifficulty ()
DECLARE SUB chooseDifficulty ()
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
DECLARE SUB exitGame ()
' ------------------------------------------
' MARS RESCUE
' ------------------------------------------
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
Dim Shared mapH(325) As Integer

'colors of mars map
Dim Shared skyBG
Dim Shared marsFG
Dim Shared starsFG

skyBG = 0 '  sky
marsFG = 4 ' land
starsFG = 7 ' stars

' ------------------------------------------
' game types
Type PLAYERtype
    x As Single
    y As Single

    ' direction x,y
    dx As Single
    dy As Single

    spdx As Single ' speed x
    spdy As Single ' speed y

    mdx As Single ' max speed x
    mdy As Single ' max speed y

    grav As Single ' gravity

    fuel As Integer
    oxygen As Integer

    score As Integer
    life As Integer

    onGround As Integer ' player on ground?

    difficulty As Integer 'how hard is the game? 1 easy 2 medium 3 hard 4 ultra
End Type

Type ASTRONAUTtype
    x As Single
    y As Single
    dx As Single
    dy As Single
    frame As Integer
    ia As Integer ' not used yet
End Type

' characters and stuff
Dim Shared MAXASTRONAUTS As Integer ' max astronauts on level
Dim Shared ACTIVEASTRONAUTS As Integer ' active astronauts on this level, when 0, go to next level!

MAXASTRONAUTS = 25 'this is very important, will determine max rescue astronauts on screen, affects performance

Dim Shared player As PLAYERtype
Dim Shared astronauts(MAXASTRONAUTS) As ASTRONAUTtype

Dim Shared currentWave ' current wave
Dim Shared maxWaves 'when reach this, the game ends
Dim Shared loopedGame
maxWaves = 12
loopedGame = 0 ' how many times won the game in this run
' ------------------------------------------
' HARDWARE SETUP
' START THE FUN
Randomize Timer
Screen 7

'only for windows - qb64
_FullScreen
If _FullScreen = 0 Then _FullScreen _Off 'check that a full screen mode initialized
_Title "Mars Rescue by Krono"
' end QB64 stuff

Do ' whole game loop #1

    ' intro
    Call IntroScreen

    Call chooseDifficulty

    Call initGame
    currentWave = 0 ' restart game

    ' easter egg when looped game
    If loopedGame > 0 Then
        skyBG = 0 '  sky
        marsFG = 5 ' land
        starsFG = 13 ' stars
    End If

    If loopedGame > 1 Then
        skyBG = 0 '  sky
        marsFG = 6 ' land
        starsFG = 15 ' stars
    End If

    If loopedGame > 2 Then
        skyBG = 0 '  sky
        marsFG = 7 ' land
        starsFG = 11 ' stars
    End If
   
    If loopedGame > 3 Then
        skyBG = 0 '  sky
        marsFG = 2 ' land
        starsFG = 10 ' stars
    End If
    ' -- end easter egg



    Do ' wave loop #2

        currentWave = currentWave + 1

        Cls
        Color 15

        ' star field
        For s = 0 To 100
            x = Rnd * 320
            y = Rnd * 200
            PSet (x, y), starsFG
        Next
        Print "MISSION #"; currentWave; " OF "; maxWaves
        Print "TRAVELING TO MARS SURFACE!"
        Print Using "MARS CREW ## }"; currentWave + 3 + player.difficulty
       
        Color 10 + player.difficulty
        Locate 22, 1
        Print "Skill: ";
        Call showDifficulty
        Print
       
        
        Color 12
        Locate 23, 1
        Print "PLEASE WAIT...FLYING TO ZONE"
        Color 15

        Call drawIntroLevel

        ' wait
        k$ = InKey$
        Sleep 3

        ' -- start level --
        ' game level setup
        Call initLevel

        ' init astronauts
        Call setupAstronauts


        ACTIVEASTRONAUTS = 3 + currentWave + player.difficulty ' ADD MORE EACH WAVE!
        If ACTIVEASTRONAUTS > MAXASTRONAUTS Then ACTIVEASTRONAUTS = MAXASTRONAUTS
        

        ' main game loop of a level #3
        wannaExit = 0 ' wait for ESC key
        Do
            'keyboard
            d$ = UCase$(InKey$)

            If d$ = Chr$(27) Then wannaExit = 1

            ' left
            If (d$ = Chr$(0) + "K" Or d$ = "A") And player.onGround = 0 Then
                player.dx = player.dx - player.spdx
                player.fuel = player.fuel - 1
            End If

            ' right
            If (d$ = Chr$(0) + "M" Or d$ = "D") And player.onGround = 0 Then
                player.dx = player.dx + player.spdx
                player.fuel = player.fuel - 1
            End If

            ' up
            If d$ = Chr$(0) + "H" Or d$ = "W" Then
                player.dy = player.dy - player.spdy
                player.fuel = player.fuel - 1
            End If

            ' down
            If d$ = Chr$(0) + "P" Or d$ = "S" Then player.dy = player.dy + player.spdy

            ' -- cheat codes
            'F9 remove astronauts
            If d$ = Chr$(0) + "C" Then ACTIVEASTRONAUTS = ACTIVEASTRONAUTS - 1
            'F10 skip level
            If d$ = Chr$(0) + "D" Then ACTIVEASTRONAUTS = 0

            ' -- end cheats --
            ' move
            ' only if not grounded on x
            If player.onGround = 0 Then player.x = player.x + player.dx

            player.y = player.y + player.dy

            ' gravity
            player.dy = player.dy + player.grav

            ' constrain
            If player.dx < -player.mdx Then player.dx = -player.mdx
            If player.dx > player.mdx Then player.dx = player.mdx
            If player.dy < -player.mdy Then player.dy = -player.mdy
            If player.dy > player.mdy * 2 Then player.dy = player.mdy * 2 'can fall twice max speed Y

            If player.x < 0 Then
                player.x = 0
                player.dx = 0
            End If

            ' this limit is quite important to not get out of bounds in map height
            If player.x > 312 Then
                player.x = 312
                player.dx = 0
            End If
           
            'dont invade the HUD
            If player.y < 16 Then
                player.y = 16
                player.dy = 0
            End If

            If player.y > 194 Then
                player.y = 194
                'player.dy = 0
            End If

            ' friction
            If player.dx < 0 Then player.dx = player.dx + player.grav
            If player.dx > 0 Then player.dx = player.dx - player.grav
            If Abs(player.dx) < player.grav Then player.dx = 0

            'check crash against ground first
            'CHECK ALL CORNERS AND MIDDLE POINT!
            mapmH = mapH(player.x + 3) 'look highest point below us in left,right,middle
            If mapH(player.x) < mapmH Then mapmH = mapH(player.x)
            If mapH(player.x + 6) < mapmH Then mapmH = mapH(player.x + 6)

            If mapmH <= player.y + 7 Then
                ' touched ground
                player.y = mapmH - 7 'clip to ground to dont go below

                'slide in big slopes
                If mapH(player.x) > player.y + 10 Then player.x = player.x - .2
                If mapH(player.x + 6) > player.y + 10 Then player.x = player.x + .2

                If player.onGround = 0 Then 'touched ground from flying down
                    Sound 100, 1 ' sound only on first touch down

                    ' check crash!
                    If player.dy > player.mdy Then
                        'debug add crash message and sound
                        Locate 10, 16
                        Color 12
                        Print "CRASH!!!"
                        Circle (player.x + 3, player.y + 4), 10, 12
                        Paint (player.x + 3, player.y + 4), 12
                        'PLAY "MFO3GGFCCo2C"
                        Play "mf o1 c c c c p32 d# d d c c b c"
                        player.life = player.life - 1

                        Call resetPlayer
                    Else
                        player.dy = 0
                        player.dx = 0
                        player.onGround = 1 'prevent X movement
                    End If
                End If
            Else
                player.onGround = 0 ' im flying
            End If

            'oxygen
            player.oxygen = player.oxygen - 1

            ' drawing game frame
            ' draw off screen
            Screen , , 1, 0
            PCopy 2, 1 ' copy background

            ' astronauts AI AND DRAWING (1 PASS)
            Call moveAstronauts

            ' do player
            Call drawPlayer

            ' HUD
            If player.life > -1 And player.fuel > 0 And player.oxygen > 0 Then
                Color 15
                Locate 1, 1
                If player.fuel < 25 Or player.oxygen < 200 Then Color 12 ' low fuel or oxygen
                Print Using ">Fuel####|Oxygen####|Score####|Ships##"; player.fuel; player.oxygen; player.score; player.life
                Locate 2, 1
                Color 15
                If player.onGround Then
                    Color 10
                    Print ">* LANDED *<"
                Else
                    Color 15
                    If player.dy > player.mdy Then Color 12 'going to crash, warn!
                    If player.dy > 0 And player.dy <= player.mdy Then Color 10 'good landing speed
                    Print Using ">dx##.# dy##.#<"; player.dx; player.dy
                End If
            Else
                Color 12
                Locate 1, 1
                Print "** MISSION FAILURE **"
                If player.life < 0 Then Print "> NO SHIPS LEFT"
                If player.oxygen < 1 Then Print "> NO OXYGEN LEFT"
                If player.fuel < 1 Then Print "> NO FUEL LEFT"
            End If

            ' flip page
            PCopy 1, 0
            Screen , , 0, 0
           
            ' -- out of fuel?
            If player.fuel < 1 Then
                player.fuel = 0
                Locate 10, 14
                Color 12
                Print "OUT OF FUEL!"
                Play "mfo2CCp32def"
                player.life = player.life - 1

                Call resetPlayer
            End If

            ' -- out of oxygen
            If player.oxygen < 1 Then
                player.oxygen = 0
                Locate 10, 13
                Color 12
                Print "OUT OF OXYGEN!"
                Play "mfo2DBDBCCC"
                player.life = player.life - 1

                Call resetPlayer
            End If

            ' -- game over --
            ' check if life less than 0 then game over
            If player.life < 0 Then
                wannaExit = 1 ' go back to main menu
                Locate 10, 12
                Color 14
                Print "MISSION FAILED!!"
                Play "mf o1 c c c c p32 d# d d c c b c p32 d# d# d# d# g f f d# d# d d# "
            End If



            ' --- high resolution timer simulation
            ' new timer delay system
            ' for QB64 or too fast computers works good too
            ' tested on DOSBOX
            idle = Timer
            While (Abs(Timer - idle) < .01)
            Wend
            ' --- end temporizer

            ' loop #3
        Loop While ACTIVEASTRONAUTS > 0 And wannaExit <> 1

        If currentWave >= maxWaves And player.life >= 0 Then
            ' won the game!
            Call showYouWon
            wannaExit = 1 'restart game anyways
        Else
            If wannaExit = 1 Then Call showScore ' end game show score
        End If
        'loop #2
    Loop While wannaExit <> 1 ' levels loop
 
    'loop #1
Loop ' big game loop of the game itself


'---------- end ------------

Sub chooseDifficulty
    Cls
    Color 14
    Print "MARS RESCUE"
    Print "==== ======"
    Print
    Print "CHOOSE YOUR DESTINY:"
    Print
    Print "1 - EASY"
    Print "2 - NORMAL"
    Print "3 - HARD"
    Print "4 - BRUTAL"
    If loopedGame > 0 Then
        Color 4
        Print "5 - SICK FUCK"
    End If
    Print
    Print
    Print
    Print
    Play "mbl8o2ccdeffa"
    Color 15
    Print "-- PRESS 1 TO 4   --"
    Color 4
    Print "-- OR ESC TO EXIT --"
    Color 15

    player.difficulty = 0
    Do
        k$ = waitForKey$

        If k$ = Chr$(27) Then Call exitGame
        If k$ = "1" Then player.difficulty = 1
        If k$ = "2" Then player.difficulty = 2
        If k$ = "3" Then player.difficulty = 3
        If k$ = "4" Then player.difficulty = 4
        If k$ = "5" Then
            player.difficulty = 5 'secret difficulty
            Color 12
            Locate 10, 1
            Print "5 - SICK FUCK"
            Play "mf o4 cdefgab o5 cdefgab"
        End If
    Loop While player.difficulty < 1
End Sub

Sub createMars
    ' creates mars background
    Line (0, 0)-(320, 200), skyBG, BF

    ' star field
    For s = 0 To 100
        x = Rnd * 320
        y = Rnd * 200
        PSet (x, y), starsFG
    Next

    y = Rnd * 50 + 100
    y2 = Rnd * 50 + 100
    segment = 6

    For x = 0 To 320 Step segment

        Line (x, y)-(x + segment, y2), marsFG

        y = y2
        y2 = y + (Rnd * 40 - 20)

        If y2 < 100 Then y2 = 100
        If y2 > 198 Then y2 = 198

    Next

    ' fill
    Paint (0, 199), marsFG


    ' scan map height
    For x = 0 To 320
        For y = 99 To 199
            c = Point(x, y)
            If c = marsFG Then
                mapH(x) = y
                Exit For
            End If
        Next
    Next
    'bugfix for when the player is too near right border
    mapH(320) = 200
    mapH(321) = 200
    mapH(322) = 200
    mapH(323) = 200
    mapH(324) = 200
    mapH(325) = 200
End Sub

Sub drawIntroLevel
    ' intro screen to a new level
    ' mars planet
    Circle (160, 100), 80, 4
    Paint (160, 100), 4
    ' mars dots
    For s = 0 To 100
        x = Rnd * 100 - 50
        y = Rnd * 100 - 50
        x = 160 - x
        y = 100 - y
        PSet (x, y), 6
        If Rnd * 50 > 40 Then Circle (x, y), 1, 6
        If Rnd * 50 > 48 Then Circle (x, y), 2, 6
        If Rnd * 50 > 48 Then Circle (x, y), 4, 6
    Next
    ' mars crew location on planet
    x = Rnd * 100 - 50
    y = Rnd * 100 - 50
    x = 160 - x
    y = 100 - y
    Line (x - 4, y - 4)-(x + 4, y + 4), 12, BF
    Line (x - 3, y - 3)-(x + 3, y + 3), 15, B
    Line (108, 20)-(x, y - 3), 15

End Sub

Sub drawPlayer
    Line (player.x + 1, player.y + 1)-(player.x + 5, player.y + 5), 10, BF
    Circle (player.x + 3, player.y + 3), 3, 2
    Line (player.x, player.y + 6)-(player.x, player.y + 7), 2
    Line (player.x + 6, player.y + 6)-(player.x + 6, player.y + 7), 2
    Line (player.x + 3, player.y + 6)-(player.x + 3, player.y + 7), 2

    'bounding box is x + 6, y  + 7
    'LINE (player.x, player.y)-(player.x + 6, player.y + 7), 15, B

    ' ---------------------
    ' draw fire from boosters
    If player.dy < 0 Then
        For i = 0 To 2 + Abs(Int(player.dy))
            PSet (player.x + Rnd * 6, player.y + 7 + Rnd * (3 + Abs(Int(player.dy)))), 14
        Next
    End If


End Sub

Sub exitGame
    ' exit game to MS DOS
    Screen 0
    Width 80, 25
    Color 12
    Print "THANKS FOR PLAYING!"
       
    Color 15
    Print "By ";
    Color 11
    Print "KRO";
    Color 15
    Print "NO";
    Color 11
    Print "MAN";
    Color 15
    Print " - (c) 2022"

    Print
    Color 10
    Print "Humans will reach Mars, and I would like to see it happen in my lifetime."
    Color 2
    Print "- Buzz Aldrin"
    Print
    Print
    Color 7
    Print "Version v1.25.11.2022 "
    End
End Sub

Sub initGame
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
End Sub

Sub initLevel

    ' draw mars off screen
    ' active page 2,view page 0
    Screen , , 2, 0 ' page 2 has the background in cache
    Call createMars
    PCopy 2, 1 ' i keep another copy in 1 to double buffer
    PCopy 1, 0 ' show to screen
    Screen , , 0, 0

    ' each new level call this
    Call resetPlayer
End Sub

Sub IntroScreen
    Cls
    Color 14
    Print "MARS RESCUE"
    Print "==== ======"
    Print

    Color 15
    Print "By ";
    Color 11
    Print "KRO";
    Color 15
    Print "NO";
    Color 11
    Print "MAN";
    Color 15
    Print " - (c) 2022"


    Color 14
    Print
    Print
    Print "THE CREW OF THE FIRST MARS BASE"
    Print "IS STRANDED ON THE SURFACE"
    Print
    Print "THEIR ONLY HOPE IS YOU!"
    Print "BRING THEM BACK ALIVE!"
    Print
    Color 15
    Print "* HOW TO PLAY *"
    Print
    Print "USE ARROW KEYS TO FIRE THRUSTERS"
    Print "YOU CAN ALSO USE A,S,D,W"
    Color 12
    Print "MONITOR YOUR DX,DY VECTOR FOR SAFETY"
    Print "DO NOT COLLIDE AGAINST MARS SURFACE"
    Color 10
    Print "RESCUE ALL THE ASTRONAUTS!"

    Color 14

    
    Print
    Print

    Play "mbl8o2ccdeffa"

    Print "-- PRESS ANY KEY  --"
    Color 4
    Print "-- OR ESC TO EXIT --"
    Color 15

    k$ = waitForKey$

    If k$ = Chr$(27) Then Call exitGame
    
End Sub

Sub moveAstronauts
    'NOTE: i also moved the draw code here to sped up loops
    'only 1 pass over astronaut list, instead of 2

    ' IA of astronauts

    For i = 0 To ACTIVEASTRONAUTS - 1

        ' constraint to screen
        If astronauts(i).x < 5 Then
            astronauts(i).x = 5
            astronauts(i).dx = -astronauts(i).dx
        End If

        If astronauts(i).x > 315 Then
            astronauts(i).x = 315
            astronauts(i).dx = -astronauts(i).dx
        End If

        If astronauts(i).y < 0 Then astronauts(i).y = 0
        If astronauts(i).y > 194 Then astronauts(i).y = 194

        ' first fall to ground
        If astronauts(i).y + 6 < mapH(astronauts(i).x + 2) Then
            astronauts(i).dy = 1.5
        Else
            astronauts(i).dy = 0 ' on ground
            astronauts(i).y = mapH(astronauts(i).x + 2) - 6
        End If
        
        astronauts(i).y = astronauts(i).y + astronauts(i).dy
        astronauts(i).x = astronauts(i).x + astronauts(i).dx
        
        ' CHECK IF THEY COLLIDED WITH PLAYER, AND REMOVE FROM LIST!
        If Abs(player.x - astronauts(i).x) < 6 And Abs(player.y - astronauts(i).y) < 10 Then
        
            Play "MBO2L8CFG" ' pickup sound
            
            player.score = player.score + 1
           
            'GIVE MORE FUEL and OXIGEN!
            player.oxygen = player.oxygen + Rnd * 10 + 5
            player.fuel = player.fuel + Rnd * 5 + 5

            If ACTIVEASTRONAUTS > 0 Then ' remove from list
                astronauts(i) = astronauts(ACTIVEASTRONAUTS - 1)
                ACTIVEASTRONAUTS = ACTIVEASTRONAUTS - 1
                Exit Sub
            Else
                ' WAVE ENDED  , is handled elsewhere
                Exit Sub
            End If
        End If

        ' will try to chase player if near, or wait
        If astronauts(i).ia < 1 Then ' only if im not running already
            If Abs(player.x - astronauts(i).x) < 50 And Abs(player.y - astronauts(i).y) < 30 Then
                astronauts(i).dx = 0 'this prevents flicker if we are just below the player ship
                If player.x < astronauts(i).x + 2 Then astronauts(i).dx = -.3
                
                If player.x + 6 > astronauts(i).x + 2 Then astronauts(i).dx = .3
            Else
                astronauts(i).dx = 0 ' wait

                If Rnd * 100 < 5 Then ' tired of waiting, move
                    astronauts(i).dx = ((Rnd * 200) - 100) / 100
                    astronauts(i).ia = Rnd * 15 + 5
                End If
            End If
        Else
            astronauts(i).ia = astronauts(i).ia - 1
        End If
        

        '-- draw code for astronaut (i)
        Line (astronauts(i).x + 2, astronauts(i).y)-(astronauts(i).x + 2, astronauts(i).y + 3), 11

        'LINE (astronauts(i).x + 2, astronauts(i).y + 2)-(astronauts(i).x + 2, astronauts(i).y + 3), 3
       
        ' animate
        If astronauts(i).frame < 10 Then
            ' arms
            Line (astronauts(i).x, astronauts(i).y)-(astronauts(i).x + 1, astronauts(i).y + 1), 3

            Line (astronauts(i).x + 3, astronauts(i).y + 1)-(astronauts(i).x + 4, astronauts(i).y), 3

            'legs
            Line (astronauts(i).x, astronauts(i).y + 5)-(astronauts(i).x + 1, astronauts(i).y + 4), 3
           
            Line (astronauts(i).x + 3, astronauts(i).y + 4)-(astronauts(i).x + 4, astronauts(i).y + 5), 3
           
        Else
            ' arms
            Line (astronauts(i).x, astronauts(i).y + 2)-(astronauts(i).x + 1, astronauts(i).y + 1), 3
           
            Line (astronauts(i).x + 3, astronauts(i).y + 1)-(astronauts(i).x + 4, astronauts(i).y + 2), 3
           
            'legs
            Line (astronauts(i).x + 1, astronauts(i).y + 5)-(astronauts(i).x + 1, astronauts(i).y + 4), 3
           
            Line (astronauts(i).x + 3, astronauts(i).y + 4)-(astronauts(i).x + 3, astronauts(i).y + 5), 3
        End If

        astronauts(i).frame = astronauts(i).frame + Rnd * 3
        If astronauts(i).frame > 20 Then astronauts(i).frame = 0

    Next
End Sub

Sub resetPlayer
    If player.difficulty < 1 Then player.difficulty = 1
   
    ' player reset for level OR LIFE LOSS
    player.x = 160
    player.y = 0
    player.dx = 0
    player.dy = 0
    player.fuel = 400 / player.difficulty
    player.oxygen = 2000 / player.difficulty
    player.onGround = 0 ' not on ground

    If player.life >= 0 Then 'only if alive
        ' GET READY MESSAGE
        ' SHOWN WHEN STARTING LEVEL OR LOSING A LIFE
        Locate 10, 10 ' erase previous messages on same line
        Print "                    "

        Locate 10, 15
        Color 10
        Print "GET READY!"
        idle = Timer
        While Abs(Timer - idle) < 1
        Wend
    End If
End Sub

Sub setupAstronauts
    ' puts astronauts on map
    For i = 0 To MAXASTRONAUTS
        astronauts(i).x = Rnd * 310 + 5
        astronauts(i).y = mapH(astronauts(i).x + 2) - 6
        astronauts(i).dx = 0
        astronauts(i).dy = 0
        astronauts(i).frame = Rnd * 20
        astronauts(i).ia = 0
    Next
    ACTIVEASTRONAUTS = MAXASTRONAUTS
End Sub

Sub showDifficulty
    Select Case player.difficulty
        Case 1
            Print "EASY";
        Case 2
            Print "NORMAL";
        Case 3
            Print "HARD";
        Case 4
            Print "BRUTAL";
        Case 5
            Print "SICK FUCK";
        Case Else
            Print "CHEAT";
    End Select

End Sub

Sub showScore
    ' shows final score, but only when the mission is a failure
    ' not for winning the game
    Cls
    Color 14
    Print "RESCUE FAILED!"
    Print "And so, mission ends..."
    Color 15
    Print
    Print "Score: "; player.score
    Print "Wave : "; currentWave
    Print "Skill: ";
    Call showDifficulty
    Print
    Print
    Color 7
    Print "Fate has ordained that the men who went to mars to explore in peace,will stay on mars to rest in peace."
    Print "- Dwayne Elizondo Mountain Dew Camacho"
    Print
    If loopedGame > 0 Then 'easter egg
        Color 8
        Print "Go touch some grass... :P"
    Else
        Print
    End If

    Print
    Print

    Play "mbl8o2dedeggo3cc"
    Color 14
    Print "-- PRESS ANY KEY TO RESTART --"
    Color 4
    Print "-- OR ESC TO EXIT           --"
    Color 15
  
    k$ = waitForKey$

    If k$ = Chr$(27) Then Call exitGame

End Sub

Sub showYouWon
    ' won the game!
    loopedGame = loopedGame + 1

    ' show when you won the game
    Cls
    Color 14
    Print "CONGRATULATIONS!!"
    Print "YOU RESCUED ALL THE COLONY!!"
    Print
    Color 15
    Print "WE ARE THE CHAMPIONS!"
    Print "== === === =========="
    Print
    Print "Score: "; player.score
    Print "Wave : "; currentWave
    Print "Skill: ";
    Call showDifficulty
    Print
    Print "Loop: "; loopedGame
    If loopedGame > 1 Then 'easter egg
        Color 13
        Print "Go touch some grass... :D"
    Else
        Print
    End If
    
    Color 7
    Print "The first human beings to land on Mars  should not come back to Earth. They     should be the beginning of a build-up of a settlement, I call it a permanence.  - Buzz Aldrin"
    Print

    'easter egg
    If player.difficulty < 5 Then
        Color 12
        Print "For a challenge, press 5 on destiny."
    End If

    Print
    Print
    Print

    Play "mbl8o2FCFCBBo3cc"
    Color 14
    Print "-- PRESS ANY KEY TO RESTART --"
    Color 4
    Print "-- OR ESC TO EXIT           --"
    Color 15
 
    k$ = waitForKey$

    If k$ = Chr$(27) Then Call exitGame
    
End Sub

Function waitForKey$
    t = Timer
    While Abs(Timer - t) < .3
        k$ = InKey$ ' flush keyboard buffer
    Wend
   
    ' real wait
    Do
        k$ = UCase$(InKey$)
    Loop Until k$ <> ""

    waitForKey$ = k$

End Function

