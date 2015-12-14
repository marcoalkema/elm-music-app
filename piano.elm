module Piano where 

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onKeyDown)
import List exposing (..)
import Signal exposing (Address)
import Notation exposing (..)

-- MODEL

init : (Int, (Int, Int), List (Int, Int))
init = (5, (0,0), [(0, 0)])

type MusicType = OctaveNumber Int
               | Note         (Note', Octave)
               | Duration     Int
               | NotatedNotes (List Int)
           
type Note = NoteC | NoteCis | NoteD | NoteDis | NoteE | NoteF | NoteFis | NoteG | NoteGis | NoteA | NoteAis | NoteB

type Event = OctaveNumberEvent   MusicType
           | KeyPressedEvent     MusicType
           | DurationButtonEvent MusicType

type alias Model = (Int, (Int, Int), List (Int, Int))
type alias Note' = Int
type alias Octave = Int

-- UPDATE

update : Event -> Model -> Model
update action (currentOctaveAmount, currentNote, currentNotationList)  = case action of
                                                                           OctaveNumberEvent   (OctaveNumber oct)    -> (oct, currentNote, currentNotationList) 
                                                                           KeyPressedEvent     (Note (newNote, oct)) -> (currentOctaveAmount, (newNote, oct), newNotationList oct newNote currentNotationList)
                                                                           DurationButtonEvent (Duration quaver)     -> (currentOctaveAmount, currentNote, currentNotationList)

newNotationList : Int -> Int -> List (Int, Int) -> List (Int, Int)
newNotationList noteOctave note currentNotationList = take 22 ((noteOctave, note)::currentNotationList)

noteToString : Note -> Int
noteToString note =
  case note of
    NoteC   -> 0
    NoteCis -> 1
    NoteD   -> 2
    NoteDis -> 3
    NoteE   -> 4
    NoteF   -> 5
    NoteFis -> 6
    NoteG   -> 7
    NoteGis -> 8
    NoteA   -> 9
    NoteAis -> 10
    NoteB   -> 11

-- VIEW

view : Address Event -> Model -> Html
view address (currentOctaveAmount, (note, octave), notationList) =
  div [containerStyle] [ div [containerStyle] (drawOctaves address currentOctaveAmount)
                       , div [notationStyle]  [text (toString (currentOctaveAmount, (note, octave), notationList))]
                       , button [onClick address (OctaveNumberEvent (OctaveNumber 1)), buttonStyle] [text "1"]
                       , button [onClick address (OctaveNumberEvent (OctaveNumber 2)), buttonStyle] [text "2"]
                       , button [onClick address (OctaveNumberEvent (OctaveNumber 3)), buttonStyle] [text "3"]
                       , button [onClick address (OctaveNumberEvent (OctaveNumber 4)), buttonStyle] [text "4"]                 
                       , button [onClick address (OctaveNumberEvent (OctaveNumber 5)), buttonStyle] [text "5"]
                       , div [notationStyle'] [fromElement (drawMusic notationList)]
                       ]
                         
drawOctave : Address Event -> Int -> Html
drawOctave address octave = div [containerStyle] (drawKeys address octave)

drawOctaves : Address Event -> Octave -> List Html
drawOctaves address currentOctaveAmount  = map (\octave -> drawOctave address octave) (octaves currentOctaveAmount)

octaves : Int -> List Int
octaves n = [1..(max 1 (abs n))]            
            
octaveNumber : Int
octaveNumber = 5
               
whiteKeyWidth : Float
whiteKeyWidth = 100 / 7
blackKeyOffset : Float
blackKeyOffset = (whiteKeyWidth * 0.7)

pos0 : (Float, Bool, Note)
pos0 = (0 * whiteKeyWidth,                     False, NoteC)
pos1 : (Float, Bool, Note)
pos1 = (blackKeyOffset + (0 * whiteKeyWidth),  True,  NoteCis)
pos2 : (Float, Bool, Note)
pos2 = (1 * whiteKeyWidth,                     False, NoteD)
pos3 : (Float, Bool, Note)
pos3 = (blackKeyOffset + (1 * whiteKeyWidth),  True,  NoteDis)
pos4 : (Float, Bool, Note)
pos4 = (2 * whiteKeyWidth,                     False, NoteE)
pos5 : (Float, Bool, Note)
pos5 = (3 * whiteKeyWidth,                     False, NoteF)
pos6 : (Float, Bool, Note)
pos6 = (blackKeyOffset + (3 * whiteKeyWidth),  True,  NoteFis)
pos7 : (Float, Bool, Note)
pos7 = (4 * whiteKeyWidth,                     False, NoteG)
pos8 : (Float, Bool, Note)
pos8 = (blackKeyOffset + (4 * whiteKeyWidth),  True,  NoteGis)
pos9 : (Float, Bool, Note)
pos9 = (5 * whiteKeyWidth,                     False, NoteA)
pos10 : (Float, Bool, Note)
pos10 = (blackKeyOffset + (5 * whiteKeyWidth), True,  NoteAis)
pos11 : (Float, Bool, Note)
pos11 = (6 * whiteKeyWidth,                    False, NoteB)

positions : List (Float, Bool, Note)
positions = [pos0, pos1, pos2, pos3, pos4, pos5, pos6, pos7, pos8, pos9, pos10, pos11]

styles : Bool -> Float -> Int -> Attribute
styles isBlack = if isBlack then styleBlack else styleWhite

drawKeys : Address Event -> Int -> List Html
drawKeys address octave = map (\(position, isBlack, note) -> drawKey address (position, isBlack, note) octave) positions

drawKey : Address Event -> (Float, Bool, Note) -> Int -> Html                          
drawKey address (position, isBlack, note) octave = (div [ onClick address (KeyPressedEvent (Note ((noteToString note), octave)))
                                                              , styles isBlack position octave] [])

-- STYLES

containerStyle : Attribute
containerStyle =
  style
    [ ("height", "100%")
    , ("width", "100%") 
    ]

whiteKeyHeight : Float
whiteKeyHeight = (6.7342 * (whiteKeyWidth / abs (toFloat octaveNumber)))

styleWhite : Float -> Int -> Attribute
styleWhite position octave =
  style
    [ ("background-color", "white")
    , ("border", "2px solid #aaa")
    , ("height", toString whiteKeyHeight ++ "%")
    , ("width", toString (whiteKeyWidth / (toFloat (abs octaveNumber))) ++ "%")
    , ("left", toString (((100 / (toFloat (abs octaveNumber)) * (toFloat octave - 1)) + (position / (toFloat (abs octaveNumber))))) ++ "%")
    , ("position", "absolute")
    ]

styleBlack : Float -> Int -> Attribute
styleBlack position octave =
  style
    [ ("background-color", "black")
    , ("color", "red")
    , ("height", (toString (0.7 * whiteKeyHeight)) ++ "%")
    , ("width", toString ((whiteKeyWidth * 0.6) / (toFloat (abs octaveNumber))) ++ "%")
    , ("z-index", "1")
    , ("position" , "absolute")
    , ("left", toString (((100 / (toFloat (abs octaveNumber)) * (toFloat octave - 1)) + (position / (toFloat (abs octaveNumber))))) ++ "%")
      ]

notationStyle : Attribute
notationStyle =
  style
    [ ("height", "100px")
    , ("width", "74.8%")
    , ("background-color", "#ddd")
    , ("border", "2px solid #aaa")
    , ("top", "200px")
    , ("position", "relative")
    , ("font-size", "72px")
    , ("float", "left")
    , ("overflow", "scroll")  
    ]

buttonStyle : Attribute
buttonStyle =
  style
    [ ("height", "104px")
    , ("width", "60px")
    , ("background-color", "#ddd")
    , ("border", "2px solid #aaa")
    , ("top", "200px")
    , ("position", "relative")
    , ("font-size", "72px")
    , ("float", "left")
    , ("left", "10px")
    , ("z-index", "4")  
    ]

notationStyle' : Attribute
notationStyle' =
  style
    [ ("height", "360px")
    , ("width", "98%")
    , ("border", "2px solid #aaa")
    , ("top", "210px")
    , ("position", "relative")
    , ("font-size", "72px")
    , ("float", "left")
    , ("left", "10px")
    , ("z-index", "4")  
    ]                  
