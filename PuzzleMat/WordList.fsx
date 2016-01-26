open System.IO

let Words = File.ReadAllLines(@"/Users/davidgross/Projects/PuzzleMat/PuzzleMat/12dicts-5.0/2of12inf.txt")
            |> Array.map ( fun (w:string) -> w.TrimEnd('%').ToUpper() )

let maxLength = Words
                |> Array.map String.length
                |> Array.max

let nLetter words n = Array.filter ( fun (w:string) -> w.Length = n ) words
let nLetterWords = nLetter Words

let WordLists = [| 0 .. maxLength |]
                |> Array.map nLetterWords