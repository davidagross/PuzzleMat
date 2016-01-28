namespace PuzzleMat

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