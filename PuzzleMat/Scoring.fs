namespace PuzzleMat

module Scoring = 

    open Types

    let ScoreString (s:string) (wordLists:string[][]) = 
        // parse away quanitifier, 1337speak, and inflate for phonemes / slang usage
        let words = s
                    |> Parsing.parseQuantifier
                    |> Language.leetNumbersToLetters
                    |> Language.inflate
        // see if remaining chars are a single word
        let valid = words
                    |> Array.map (fun (w:string) -> Array.exists (fun v -> System.String.Equals(w,v) ) wordLists.[w.Length] )
        match (valid |> Array.contains true) with
        | true -> 
            // let idx = Array.findIndex (id) valid
            // printf "%s" words.[idx]
            let validWords = Array.zip valid words |> Array.filter fst |> Array.map snd
            printf "%s - " s
            ignore ( validWords |> Array.map (printf "%s ") )
            printf "\n"
            1
        | false -> 0

        // see if other partitions are parseable (with quantifiers there too?)

    let ScoreGrid (grid:Grid) (wordLists:string[][]) = 
        let scoreSentenceArray = Array.fold (fun (i:int) (s:string) -> i + ScoreString s wordLists) 0
        let rowScore = scoreSentenceArray grid.Rows
        let colScore = scoreSentenceArray grid.Columns
        100.0*((float)rowScore + (float)colScore)/((float)grid.Rows.Length + (float)grid.Columns.Length)