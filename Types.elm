module Types where

type alias OctaveNumber = Int
type alias NoteNumber   = Int
type alias Octave       = Int

type Duration = Eighth
              | Quarter
              | Half
              | Whole                                 

type alias Note = (Octave, NoteNumber, Duration)

type Event = OctaveNumberEvent   OctaveNumber
           | KeyPressedEvent     Note
           | DurationButtonEvent Duration

type alias Model = (OctaveNumber, Note, List Note)

type alias IsBlack = Bool
type Note' = NoteC | NoteCis | NoteD | NoteDis | NoteE | NoteF | NoteFis | NoteG | NoteGis | NoteA | NoteAis | NoteB
