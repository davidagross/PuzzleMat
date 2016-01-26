namespace PuzzleMat

open FParsec

module Types = 

    type Row = string
    type Column = string
    type Mat = string
    type Grid = { Rows:Row array ; Columns:Column array }

module Funs = 

    open Types

    let Reshape (mat:Mat) (numCols:int) = 

        let cols = Array.zeroCreate<Column> numCols
        let rows = 
            match mat.Length % numCols with
            | 0 -> Array.zeroCreate<Row> (mat.Length/numCols)
            | _ -> Array.zeroCreate<Row> (1+(mat.Length/numCols))
        
        // let Mat be stored in a row-major format
        for r = 0 to rows.Length-2 do
            rows.[r] <- mat.[r*numCols..(r+1)*numCols-1];
        rows.[rows.Length-1] <- mat.[(rows.Length-1)*numCols..mat.Length-1]

        for c = 0 to numCols-1 do
            cols.[c] <- Column ((mat.ToCharArray())
                |> Array.mapi (fun i (x:char) -> (i-c,x)) 
                |> Array.filter ( fun (i,x) -> i % numCols = 0 )
                |> Array.map snd)

        { Rows = rows ; Columns = cols }

    let ScoreString (s:string) (wordLists:string[][]) = 
        0

    let ScoreGrid (grid:Grid) (wordLists:string[][]) = 
        let scoreSentenceArray = Array.fold (fun (i:int) (s:string) -> i + ScoreString s wordLists) 0
        let rowScore = scoreSentenceArray grid.Rows
        let colScore = scoreSentenceArray grid.Columns
        100.0*((float)rowScore + (float)colScore)/((float)grid.Rows.Length + (float)grid.Columns.Length)