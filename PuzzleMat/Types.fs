namespace PuzzleMat

module Types = 

    type Row = string
    type Column = string
    type Dim = 
        | Rows
        | Columns
    type Mat = string
    type Grid = { Rows:Row array ; Columns:Column array }

    type Phoneme = string[]

    type Inflater = string -> string[]