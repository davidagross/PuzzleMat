namespace PuzzleMat

module Optimize = 

    open Types

    module GridManipulation = 

        let IterateRule (grid:Grid) (wordLists:string[][]) (rule:Grid * System.Random -> Grid) = 
            let mutable bestGrid = grid
            let mutable bestScore = Scoring.ScoreGrid grid wordLists
            let mutable continueLooping = true
            let mutable count = 0
            let rnd = System.Random()
            while continueLooping do
                let thisGrid = rule (bestGrid, rnd)
                let thisScore = Scoring.ScoreGrid thisGrid wordLists
                count <- count + 1
                if thisScore > bestScore then count <- 0
                if thisScore >= bestScore then
                    bestScore <- thisScore
                    bestGrid <- thisGrid
                    Manipulation.print bestGrid
                    printf " ... with score of %f\n" thisScore
                if thisScore > 85.0 || count > grid.Rows.Length * grid.Rows.Length * grid.Columns.Length * grid.Columns.Length then
                    continueLooping <- false
            bestGrid    

        let SwapRowsAndColumns (grid:Grid , rnd:System.Random) = 
            match (rnd.Next 2) with
                | 0 -> Manipulation.Swap grid Dim.Rows (rnd.Next grid.Rows.Length) (rnd.Next grid.Rows.Length)
                | 1 -> Manipulation.Swap grid Dim.Columns (rnd.Next grid.Columns.Length) (rnd.Next grid.Columns.Length)
                | _ -> failwith "rnd.Next(2) failed to provide only 0 or 1" 

        let MergeRowsAndColumns (grid:Grid , rnd:System.Random) = 
            match (rnd.Next 2) with
                | 0 -> Manipulation.Merge grid Dim.Rows (rnd.Next grid.Rows.Length) (rnd.Next grid.Rows.Length) (rnd.Next grid.Rows.Length)
                | 1 -> Manipulation.Merge grid Dim.Columns (rnd.Next grid.Columns.Length) (rnd.Next grid.Columns.Length) (rnd.Next grid.Columns.Length)
                | _ -> failwith "rnd.Next(2) failed to provide only 0 or 1" 

    // from http://stefanalfbo.github.io/blog/2014/03/16/genetic-algorithms-with-fsharp/
    module Genetic = 

        type Chromosome = Mat
        type Fitness = int
        type Individual = Chromosome * Fitness

        let eliteSize = 10

        // let calculateFitness (chromosome:Chromosome) = 