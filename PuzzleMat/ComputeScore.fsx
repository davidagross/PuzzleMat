#r "/Users/davidgross/Projects/PuzzleMat/packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "/Users/davidgross/Projects/PuzzleMat/packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
#load "PuzzleMat.fs"
#load "WordList.fsx"

open PuzzleMat.Funs
open PuzzleMat.Types
open System
open WordList

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
let mat = scramble abc
let grid = Reshape mat 6

let actual = "8FROGS69PHUX5WACKZ2B0NEY4D3V1L7QTJIM"
ScoreGrid (Reshape actual 6) WordLists