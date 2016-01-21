namespace PuzzleMat

module PuzzleMatTypes = 

    type Row = Row of string
    type Column = Column of string

    type Mat = 
        {
            Letters:string ; 
            Rows:int ; 
            Columns: int
        }