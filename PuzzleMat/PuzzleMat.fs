namespace PuzzleMat

module Types = 

    type Row = string
    type Column = string
    type Mat = string
    type Grid = { Rows:Row array ; Columns:Column array }
    type Phoneme = string[]
    type Inflater = string -> string[]

module Parsing = 

    open System.Text.RegularExpressions

    // ParseRegex parses a regular expression and returns a list of the strings that match each group in
    // the regular expression.
    // List.tail is called to eliminate the first element in the list, which is the full matched expression,
    // since only the matches for each group are wanted.
    let (|ParseRegex|_|) regex str =
       let m = Regex(regex).Match(str)
       if m.Success
       then Some (List.tail [ for x in m.Groups -> x.Value ])
       else None
    
    let (|Integer|_|) (str: string) =
        let mutable intvalue = 0
        if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
        else None

    let parseQuantifier str =
       match str with
         | ParseRegex "([^A-Z]+)([A-Z0-9]*)$" [Integer n; s] -> s
         | _ -> str

module Language = 

    open Types

    // from http://www.ninjalane.com/leet.aspx
    let leetNumberToLetter (c:char) = 
        match c with 
        | '0' -> 'O'
        | '1' -> 'I'
        | '2' -> 'Z'
        | '3' -> 'E'
        | '4' -> 'A'
        | '5' -> 'S'
        | '6' -> 'G'
        | '7' -> 'T'
        | '8' -> 'B'
        | '9' -> 'G'
        | _ -> c

    let leetNumbersToLetters (s:string) = 
        s.ToCharArray() 
        |> Array.map leetNumberToLetter
        |> System.String.Concat

    // from https://en.wikipedia.org/wiki/Help:IPA_for_English
    let phonemesAndSlang = [|
        [| "TH" ; "THE" |] ;                        // ð
        [| "G" ; "DGE" ; "J" |] ;                   // dʒ
        [| "F" ; "FF" ; "PH" |] ;                   // f
        // [| "Y" ; "J" |] ;                           // j
        [| "K" ; "C" ; "CK" |] ;                    // k 
        [| "NG" ; "N" |] ;                          // ŋ
        [| "S" ; "SS" |] ;                          // s
        [| "SH" ; "TI" |] ;                         // ʃ
        [| "CH" ; "TCH" |] ;                        // tʃ
        [| "V" ; "VE" |] ;                          // v
        [| "Z" ; "S" |] ;                           // z
        //[| "TI" ; "S" ; "SI" ; "G" |] ;             // ʒ
        [| "GH" ; "CH" |] ;                         // x
        [| "ON" ; "ANT" |] ;                        // ˜
        [| "I" ; "IE" |] ;                          // aɪ
        [| "OU" ; "OW" |] ;                         // aʊ
        [| "A" ; "AI" ; "EI" ; "AY" |] ;            // eɪ
        [| "EE" ; "EA" |] ;                         // iː
        [| "OUGH" ; "AU" ; "AW" ; "A" |] ;         // ɔː
        [| "OI" ; "OY" |] ;                         // ɔɪ
        [| "OA" ; "O" |] ;                          // oʊ
        [| "OO" ; "U" ; "O" |] ;                    // ʊ
        [| "OO" ; "EW" ; "O" |] ;                   // uː
        [| "U" ; "EAU" ; "YOU" |] ;                 // juː
        [| "ARE" ; "AR" ; "AIR" |] ;                // ɛər
        [| "EAR" ; "IER" ; "ER" |] ;                // ɪər
        [| "OR" ; "AR" ; "AUR" |] ;                 // ɔːr
        [| "OR" ; "ORE" ; "OAR" |] ;                // ɔər
        [| "OOR" ; "OUR" |] ;                       // ʊər
        [| "UR" ; "OR" ; "IR" ; "ER" ; "URR" |] ;   // ɜːr
        [| "URR" ; "OUR" |] ;                       // ʌr
        [| "X" ; "CKS" ; "KS" ; "GTS" ; "CKZ" ; "KZ" ; "GTZ" |]
        |]

    let collapsePhonemes (phonemes:Phoneme[]) = 

        let comparePhonemes (a:Phoneme) (b:Phoneme) = 
            a 
            |> Array.map ( fun x -> Array.contains x b)
            |> Array.contains true
        
        let mergePhonemes (a:Phoneme) (b:Phoneme) = 
            Array.append a b |> Array.distinct

        let folder (collapsedPhonemes:Phoneme[]) (p:Phoneme) =
            let phonemePresent = collapsedPhonemes
                                 |> Array.map (comparePhonemes p)
            match (phonemePresent |> Array.contains true) with
            | true -> 
                let idx = Array.findIndex (id) phonemePresent
                collapsedPhonemes.[idx] <- mergePhonemes collapsedPhonemes.[idx] p
                collapsedPhonemes
            | false -> Array.append collapsedPhonemes [| p |]
        
        phonemes |> Array.fold folder Array.empty<Phoneme>

    let inflatePhoneme (p:Phoneme) (s:string) =
        let phonemeUsed = p |> Array.map s.Contains
        match (phonemeUsed |> Array.contains true) with
        | true -> 
            let idx = Array.findIndex (id) phonemeUsed
            p |> Array.map (fun x -> s.Replace(p.[idx] , x))
        | false -> [| s |]
     
    let inflatePhonemes (s:string) (phonemes:Phoneme[]) = 
        let folder = fun (s:string[]) (i:Inflater) -> Array.map i s |> Array.concat
        phonemes
        |> Array.map inflatePhoneme
        |> Array.fold folder [|s|]

    let inflate (s:string) = 
        collapsePhonemes phonemesAndSlang
        |> inflatePhonemes s

module Funs = 

    open Types
    open Parsing
    open Language

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
        // parse away quanitifier, 1337speak, and inflate for phonemes / slang usage
        let words = s
                    |> parseQuantifier
                    |> leetNumbersToLetters
                    |> inflate
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