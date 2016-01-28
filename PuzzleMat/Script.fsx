#load "Types.fs"
#load "Parsing.fs"
#load "Language.fs"
#load "Scoring.fs"
#load "Manipulation.fs"
#load "Optimize.fs"
#load "WordList.fsx"

open PuzzleMat
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

// 12 columns somtimes gives:
//System.IndexOutOfRangeException: Index was outside the bounds of the array.
//  at FSI_0005.PuzzleMat.Scoring.f@1-3 (System.String[][] wordLists, System.String w) <0x5dedf60 + 0x00037> in <filename unknown>:0 
//  at FSI_0005.PuzzleMat.Scoring.ScoreString (System.String s, System.String[][] wordLists) <0x5dcac38 + 0x000d3> in <filename unknown>:0 
//  at FSI_0005.PuzzleMat.Scoring+scoreSentenceArray@165-2.Invoke (Int32 i, System.String s) <0x5dcac00 + 0x0001b> in <filename unknown>:0 
//  at Microsoft.FSharp.Collections.ArrayModule.Fold[T,TState] (Microsoft.FSharp.Core.FSharpFunc`2 folder, Microsoft.FSharp.Collections.TState state, Microsoft.FSharp.Collections.T[] array) <0x5dcaaa8 + 0x00097> in <filename unknown>:0 
//  at FSI_0005.PuzzleMat.Scoring.ScoreGrid (FSI_0005.PuzzleMat.Grid grid, System.String[][] wordLists) <0x5dca908 + 0x00068> in <filename unknown>:0 
//  at <StartupCode$FSI_0007>.$FSI_0007.main@ () <0x5dc9fa8 + 0x0004f> in <filename unknown>:0 
//  at (wrapper managed-to-native) System.Reflection.MonoMethod:InternalInvoke (System.Reflection.MonoMethod,object,object[],System.Exception&)
//  at System.Reflection.MonoMethod.Invoke (System.Object obj, BindingFlags invokeAttr, System.Reflection.Binder binder, System.Object[] parameters, System.Globalization.CultureInfo culture) <0x1a55c40 + 0x000a1> in <filename unknown>:0 
//Stopped due to error 
let mat = Manipulation.Reshape (scramble "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") 6
let randomScore = Scoring.ScoreGrid mat WordLists

let actual = Manipulation.Reshape "8FROGS69PHUX5WACKZ2B0NEY4D3V1L7QTJIM" 6
let actualScore = Scoring.ScoreGrid actual WordLists

let found = Manipulation.Reshape "8XFH62Z9P45DWANGBIEQCJUV1703ORMLKSYT" 6
let foundScore = Scoring.ScoreGrid found WordLists

let best = Optimize.GridManipulation.IterateRule mat WordLists Optimize.GridManipulation.MergeRowsAndColumns;;
let bestScore = Scoring.ScoreGrid best WordLists