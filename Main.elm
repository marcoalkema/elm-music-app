import Html exposing (text)
import String exposing (fromList)
import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

main : Element
main = drawMusic

-- Draw form elements

drawMusic = collage 1000 600
                   [ staves
                   , drawNotes
                   ]

-- MELODY INPUT
           
nootjes = [0, 0, 4, 5, 6, 6, 5]


-- Define vertical note direction through pivot (The note B, or middle beam on staff)

notePivot y = y > 19.5

noteVertical x y = if notePivot y then
                    noteUp x y
                   else
                    noteDown x y
                             
spaceBetweenNotes x y = move (30 * toFloat x, y)          
noteUp x y   = eightNoteDown |> spaceBetweenNotes x y
noteDown x y = eightNoteUp   |> spaceBetweenNotes x y

-- Every step up or down in melody equals 6.5 up or down on canvas (Vertical draw)
                            
notesHorizontalAlign = map (\x -> x * 6.5) nootjes

-- Group notes in one form
        
drawNotes = group (indexedMap (\i y -> noteVertical i y) notesHorizontalAlign)
                            
-- Draw notes/note-elements
        
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
noteFlagUpPoly      : List (Float, Float)                   
noteFlagUpPoly    = [ (-400, 165)
                  , (-350 , 120)
                  , (-350 , 55)
                  , (-380 , 30)
                  , (-380 , 40)
                  , (-360 , 60) 
                  , (-360 , 55)
                  , (-360 , 105)
                  , (-400 , 130) ]
noteFlagDownPoly  = map (\(x,y) -> (x, 0-y)) noteFlagUpPoly
noteFlag updown x y = polygon updown
                     |> filled clearBlack
                     |> scale 0.2
                     |> move (-365 - x,64 - y)
noteFlagUp          = noteFlag noteFlagUpPoly 0 0
noteFlagDown        = noteFlag noteFlagDownPoly 10 1

-- Group individual note elements
                      
wholeNote       = group [noteHead, noteHeadWhole]
halfNoteUp      = group [noteHead, noteHeadHalf, noteBeamUp]
halfNoteDown    = group [noteHead, noteHeadHalf, noteBeamDown]
quarterNoteUp   = group [noteHead, noteBeamUp]
quarterNoteDown = group [noteHead, noteBeamDown]
eightNoteUp     = group [noteHead, noteBeamUp,   noteFlagUp]
eightNoteDown   = group [noteHead, noteBeamDown, noteFlagDown]

-- Individual beams for staff.

beam     = rect 6300 13
        |> filled clearGrey
        |> move (150, 57)
        |> scale 0.2

-- Create staves
           
staff offset = group (indexedMap (\i y-> (beam |> move (0, offset +  13 * toFloat i ))) (repeat 5 []))
staves = group [staff 0, staff -78]


-- Get note and octave from MIDI-note number

-- notes = [60, 62, 64]
-- z = 62
-- getNote = z % 12
-- getOctave = (z - (z % 12)) // 12


-- Colors
clearGrey : Color
clearGrey = rgba 122 122 122 1
  
clearBlack : Color
clearBlack = rgba 000 000 000 1
