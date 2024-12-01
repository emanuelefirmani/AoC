module AdventOfCode.FileSplitter

open System

let splitInLines (text: string) = text.Split('\n')
let splitLine f (line: string) = line.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> f

let mapToTwoInts (items: string array) =
    [items[0] |> int; items[1] |> int]

let splitInLinesInTwoInts text =
    splitInLines text
    |> Array.map (splitLine mapToTwoInts)
    |> List.ofArray

let splitList (l: 'a list list) (col: int) : 'a list =
    l |> List.map (fun r -> r[col])