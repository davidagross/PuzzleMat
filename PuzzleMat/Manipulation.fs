namespace PuzzleMat

module Manipulation = 

    open Types

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

    let print (grid:Grid) = 
        let banner = System.String.Concat (Array.create grid.Rows.Length '-')
        printf "+%s+\n" banner
        for r = 0 to grid.Rows.Length-1 do
            printf "|%s|\n" grid.Rows.[r]
        printf "+%s+\n" banner

    let Swap (grid:Grid) (dim:Dim) (i:int) (j:int) = 
        match dim with
        | Rows -> 
            // swap the rows
            let rows = Array.copy grid.Rows
            let tmp = rows.[i]
            rows.[i] <- rows.[j]
            rows.[j] <- tmp
            // re-construct columns based on rows by reshaping row-major unzipped grid
            // TODO DOES NOT FAIL WHEN NOT SQUARE
            Reshape (System.String.Concat rows) grid.Columns.Length
        | Columns ->
            // swap the columns
            let cols = Array.copy grid.Columns
            let tmp = cols.[i]
            cols.[i] <- cols.[j]
            cols.[j] <- tmp
            // re-construct the rows manually because why not
            let numRows = grid.Rows.Length
            let rows = Array.zeroCreate<Row> numRows
            for r = 0 to numRows-2 do
                rows.[r] <- Row (cols |> Array.map (fun c -> c.[r]))
            // TODO DOES NOT FAIL WHEN NOT SQUARE
            rows.[numRows-1] <- Row (cols.[0..(grid.Rows.[numRows-1].Length-1)] |> Array.map (fun c -> c.[numRows-1]))
            { Rows = rows ; Columns = cols }

    let Merge (grid:Grid) (dim:Dim) (i:int) (j:int) (k:int) = 
        match dim with
        | Rows -> 
            // swap the rows
            let rows = Array.copy grid.Rows
            let tmpi = rows.[i]
            let tmpj = rows.[j]
            rows.[i] <- tmpi.[0..k] + tmpj.[k+1..]
            rows.[j] <- tmpj.[0..k] + tmpi.[k+1..]
            // re-construct columns based on rows by reshaping row-major unzipped grid
            // TODO DOES NOT FAIL WHEN NOT SQUARE
            Reshape (System.String.Concat rows) grid.Columns.Length
        | Columns ->
            // swap the columns
            let cols = Array.copy grid.Columns
            let tmpi = cols.[i]
            let tmpj = cols.[j]
            cols.[i] <- tmpi.[0..k] + tmpj.[k+1..]
            cols.[j] <- tmpj.[0..k] + tmpi.[k+1..]
            // re-construct the rows manually because why not
            let numRows = grid.Rows.Length
            let rows = Array.zeroCreate<Row> numRows
            for r = 0 to numRows-2 do
                rows.[r] <- Row (cols |> Array.map (fun c -> c.[r]))
            // TODO DOES NOT FAIL WHEN NOT SQUARE
            rows.[numRows-1] <- Row (cols.[0..(grid.Rows.[numRows-1].Length-1)] |> Array.map (fun c -> c.[numRows-1]))
            { Rows = rows ; Columns = cols }