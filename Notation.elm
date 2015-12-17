module Notation where

import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Types exposing (..)

-- Draw form elements

                 
drawMusic notationList = collage 1400 400
                   [ staves
                     |> scale 1.7
                     |> move (370, -55)
                   , drawNotes notationList
                     |> scale 1.7
                     |> move (170, -385)
                   ]

drawNotes : List Note -> Form
drawNotes notationList = group (indexedMap (\horizontalIndex (verticalPosition, duration) -> noteVerticalAlign (notePivot verticalPosition) duration |> spaceBetweenNotes verticalPosition horizontalIndex) (notesHorizontalAlign notationList))

notesHorizontalAlign : List Note -> List (Float, Duration)
notesHorizontalAlign = map (\(octave, note, duration) -> ((6.5 * toFloat note) + (6.5 * (12 * toFloat octave)), duration))

noteVerticalAlign : Bool -> Duration -> Form
noteVerticalAlign isNotePivot duration =
  if isNotePivot then
    case duration of
      Eighth  -> eightNoteDown
      Quarter -> quarterNoteDown
      Half    -> halfNoteDown
      Whole   -> wholeNote       
  else
    case duration of
      Eighth  -> eightNoteUp
      Quarter -> quarterNoteUp
      Half    -> halfNoteUp
      Whole   -> wholeNote

notePivot : comparable -> Bool
notePivot verticalPosition = verticalPosition > 137
                                                                     
spaceBetweenNotes : Float -> Int -> Form -> Form
spaceBetweenNotes verticalPosition horizontalPosition = move (30 * toFloat horizontalPosition, verticalPosition)          

-- Notation Elements                           
        
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
noteFlagDownPoly  = map (\(x,y) -> (x, 0 - y)) noteFlagUpPoly
noteFlag updown x y = polygon updown
                     |> filled clearBlack
                     |> scale 0.2
                     |> move (-365 - x,64 - y)
noteFlagUp          = noteFlag noteFlagUpPoly 0 0
noteFlagDown        = noteFlag noteFlagDownPoly 10 1

-- Group individual note elements

wholeNote : Form
wholeNote       = group [noteHead, noteHeadWhole]
halfNoteUp : Form
halfNoteUp      = group [noteHead, noteHeadHalf, noteBeamUp]
halfNoteDown : Form
halfNoteDown    = group [noteHead, noteHeadHalf, noteBeamDown]
quarterNoteUp : Form
quarterNoteUp   = group [noteHead, noteBeamUp]
quarterNoteDown : Form
quarterNoteDown = group [noteHead, noteBeamDown]
eightNoteUp : Form
eightNoteUp     = group [noteHead, noteBeamUp,   noteFlagUp]
eightNoteDown : Form
eightNoteDown   = group [noteHead, noteBeamDown, noteFlagDown]

-- Individual beams for staff.

beam : Form
beam     = rect 3400 13
        |> filled clearGrey
        |> move (-260, 57)
        |> scale 0.2

-- Create staves

staff : Float -> Form
staff offset = group (indexedMap (\i y-> (beam |> move (0, offset +  13 * toFloat i ))) (repeat 5 []))

staves : Form
staves = group [staff 0, staff -78]

-- Colors

clearGrey : Color
clearGrey = rgba 122 122 122 1
  
clearBlack : Color
clearBlack = rgba 000 000 000 1
