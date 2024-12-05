module AdventOfCode.D04.Resolution

open System
open AdventOfCode.FileSplitter

type direction = { x: int; y: int }

let directions: direction array =
    [|
       { x = 1; y = 0 }
       { x = 1; y = -1 }
       { x = 0; y = -1 }
       { x = -1; y = -1 }
       { x = -1; y = 0 }
       { x = -1; y = 1 }
       { x = 0; y = 1 }
       { x = 1; y = 1 }
    |]

let getWord (matrix: string array array) r c (direction: direction) =
    if(0 <= r + direction.x * 3 && r + direction.x * 3 < matrix.Length ) then
        if(0 <= c + direction.y * 3 && c + direction.y * 3 < matrix[r].Length ) then
            [0..3]
            |> Seq.map (fun i -> matrix[r + direction.x * i][c + direction.y * i])
            |> String.Concat
        else
            ""
    else
        ""

let getWords (matrix: string array array) r c =
    directions
    |> Seq.map (fun d -> getWord matrix r c d)
    |> Seq.filter (fun x -> x <> "")

let private countXmasFromCell (matrix: string array array) r c =
    if(matrix[r][c] <> "X") then
        0
    else
        getWords matrix r c
        |> Seq.filter (fun w -> w = "XMAS")
        |> Seq.length

let getWordsAsX (matrix: string array array) r c =
    seq {
        yield matrix[r-1][c-1] + matrix[r][c] + matrix[r+1][c+1] 
        yield matrix[r+1][c-1] + matrix[r][c] + matrix[r-1][c+1] 
    }

let private countMasAsXFromCell (matrix: string array array) r c =
    if(matrix[r][c] <> "A") then
        0
    else
        if(
            r = 0 || r >= matrix.Length - 1 ||
            c = 0 || c >= matrix[r].Length - 1
            ) then
            0
        else
            if (getWordsAsX matrix r c |> Seq.forall (fun w -> w = "MAS" || w = "SAM")) then
                1
            else
                0

let private countByRow f (matrix: string array array) r =
    [ 0 .. (matrix[r].Length - 1) ]
    |> Seq.map (f matrix r)
    |> Seq.sum

let private toStringArray chars = chars |> Array.map _.ToString()
let private toMatrix input =
    input
    |> splitInLines
    |> Array.map (fun l -> toStringArray (l.ToCharArray()))

let countXmas input =
    let matrix = toMatrix input

    [ 0 .. (matrix.Length - 1) ]
    |> Seq.map (countByRow countXmasFromCell matrix)
    |> Seq.sum

let countMasAsX input =
    let matrix = toMatrix input

    [ 1 .. (matrix.Length - 2) ]
    |> Seq.map (countByRow countMasAsXFromCell matrix)
    |> Seq.sum
