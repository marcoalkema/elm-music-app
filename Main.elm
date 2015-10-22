import Html exposing (text)
import String exposing (fromList)
import List exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

main : Element
main = show snip

-- drawMusic = List.append staff notesList

drawbeam = collage 1000 600
                   [ staves
                   , xAxis
                   ]

nootjes = [0, 0, 4, 5, 6, 6, 5]

notePivot y = y > 19.5

noteVertical x y = if notePivot y then
                    noteUp x y
                   else
                    noteDown x y
spaceBetweenNotes x y = move (30 * toFloat x, y)          
noteUp x y   = eightNoteDown |> spaceBetweenNotes x y
noteDown x y = eightNoteUp   |> spaceBetweenNotes x y
               
yAxis = map (\x -> x * 6.5) nootjes
xAxis = group (indexedMap (\i y -> noteVertical i y) yAxis)

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

-- Functie moet eerste set (coordinaten) van de lijst nemen als startpunt.
-- Functie moet door de lijst itereren en het verschil tussen de huidige en de volgende set berekenen en deze omzetten in negatieve waarden (spiegeling).
-- 
-- Stap 1: Verschil tussen twee sets berekenen en deze omzetten in eigen set
-- 

xys  : List (Int, Int)
xys  = unzip noteFlagPoly
xs   = fst xys
ys   = snd xys
snip = map2 (+) (head xs) (tail (head xs))
       

wholeNote       = group [noteHead, noteHeadWhole]
halfNoteUp      = group [noteHead, noteHeadHalf, noteBeamUp]
halfNoteDown    = group [noteHead, noteHeadHalf, noteBeamDown]
quarterNoteUp   = group [noteHead, noteBeamUp]
quarterNoteDown = group [noteHead, noteBeamDown]
eightNoteUp     = group [noteHead, noteBeamUp,   noteFlag]
eightNoteDown   = group [noteHead, noteBeamDown, noteFlag]

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
