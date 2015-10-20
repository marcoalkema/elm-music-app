import Html exposing (text)
import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

main : Element
main = drawbeam

-- drawMusic = List.append staff notesList

drawbeam = collage 1000 600
                   [ staff2
                   , staff
                   , xAxis
                   ]

nootjes = [0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -2]

noteVertical x y = if x (>) 26 then
                    noteUp
                   else
                    noteDown
          
noteUp x y   = eightNoteUp |> move (30 * toFloat x, y)
noteDown x y = halfNoteUp |> move (30 * toFloat x, y)
               
yAxis = map (\x -> x * 6.5) nootjes
xAxis = group (indexedMap (\i y -> noteUp i y) yAxis)

-- Quarter Notes
        
noteHeadWhole   = oval 45 30
                |> filled white
                |> move (-450,63)
                |> scale 0.2
                |> rotate (degrees 100)  
noteHeadHalf   = oval 60 20
                |> filled white
                |> move (-450,63)
                |> scale 0.2
                |> rotate (degrees 30)  
noteHead        = oval 80 50
                |> filled clearBlack
                |> move (-450,63)
                |> scale 0.2
                |> rotate (degrees 30)  
noteBeamUp      = rect 13 160
                |> filled clearBlack
                |> move (-444,81)
                |> scale 0.2
noteBeamDown    = rect 13 160
                |> filled clearBlack
                |> move (-455.9,46)
                |> scale 0.2
noteFlagPoly      = [ (-400, 165)
                  , (-350 , 120)
                  , (-350 , 55)
                  , (-380 , 30)
                  , (-380 , 40)
                  , (-360 , 60) 
                  , (-360 , 55)
                  , (-360 , 105)
                  , (-400 , 130) ] 
noteFlag        = polygon noteFlagPoly
                  |> filled clearBlack
                  |> scale 0.2
                  |> move (-365,64)                  

                   
wholeNote       = group [noteHead, noteHeadWhole]
halfNoteUp      = group [noteHead, noteHeadHalf, noteBeamUp]
halfNoteDown    = group [noteHead, noteHeadHalf, noteBeamDown]
quarterNoteUp   = group [noteHead, noteBeamUp]
quarterNoteDown = group [noteHead, noteBeamDown]
eightNoteUp   = group [noteHead, noteBeamUp,   noteFlag]
eightNoteDown = group [noteHead, noteBeamDown, noteFlag]

-- Individual beams for staff.

beam     = rect 6300 13
        |> filled clearGrey
        |> move (150, 57)
        |> scale 0.2

-- Create staves
           
staff  = group (indexedMap (\i y-> (beam |> move (0, 13 * toFloat i ))) (repeat 5 []))
staff2 = group (indexedMap (\i y-> (beam |> move (0, -78  + 13 * toFloat i ))) (repeat 5 []))

-- Get note and octave from MIDI-note number
notes = [60, 62, 64]
z = 62
getNote = z % 12
getOctave = (z - (z % 12)) // 12


-- Colors
clearGrey : Color
clearGrey = rgba 122 122 122 1
  
clearBlack : Color
clearBlack = rgba 000 000 000 1
# elm-music-app
