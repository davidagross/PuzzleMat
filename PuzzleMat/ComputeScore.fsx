#load "PuzzleMat.fs"
#load "WordList.fsx"

open PuzzleMat.Funs
open PuzzleMat.Language
open PuzzleMat.Parsing
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

let mat = Reshape (scramble "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") 6
let randomScore = ScoreGrid mat WordLists

let actual = Reshape "8FROGS69PHUX5WACKZ2B0NEY4D3V1L7QTJIM" 6
let actualScore = ScoreGrid actual WordLists