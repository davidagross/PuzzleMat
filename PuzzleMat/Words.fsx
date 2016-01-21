#load "PuzzleMat.fs"

open System
open System.IO

Environment.CurrentDirectory <- @"/Users/davidgross/Projects/PuzzleMat/PuzzleMat"

let Words = File.ReadAllLines("12dicts-5.0/2of12inf.txt")
            |> Array.map ( fun (w:string) -> w.TrimEnd('%').ToUpper() )

let maxLength = Words
                |> Array.map String.length
                |> Array.max

let nLetter words n = Array.filter ( fun (w:string) -> w.Length = n ) words
let nLetterWords = nLetter Words

let WordLists = [| 0 .. maxLength |]
                |> Array.map nLetterWords

let scramble (letters:string) = 
    let mutable scrambled = letters
    let rnd = Random()
    for i = letters.Length-1 downto 1 do
        let j = rnd.Next(i)
        let I = scrambled.[i]
        let J = scrambled.[j]
        scrambled <- scrambled.Replace(I,'*').Replace(J,I).Replace('*',J)
    scrambled

let abc = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
let actual = "8FROGS69PHUX5WACKZ2B0NEY4D3V1L7QTJIM"

let mat = scramble abc

let grid = PuzzleMat.Funs.Reshape mat 6