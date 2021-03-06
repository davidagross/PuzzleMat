﻿namespace PuzzleMat

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
         | ParseRegex "^([^A-Z]+)([A-Z0-9]*)$" [Integer n; s] -> s
         | _ -> str