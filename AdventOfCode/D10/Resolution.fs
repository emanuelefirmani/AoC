module AdventOfCode.D10.Resolution

open System
open AdventOfCode

type Coordinates = { x: int; y: int }
type TrailHead = { coordinates: Coordinates }
type TrailItem = { coordinates: Coordinates; height: int }
type TrailSummit = { coordinates: Coordinates }

type Position =
    | TrailHead of TrailHead
    | TrailSummit of TrailSummit 
    | TrailItem of TrailItem

let isHead =
    function
    | TrailHead _-> true
    | _ -> false

let toPosition y x c =
    let coordinates = { x = x; y = y }

    match c with
    | '0' -> TrailHead { coordinates = coordinates }
    | '9' -> TrailSummit { coordinates = coordinates }
    | _ -> TrailItem { coordinates = coordinates; height = Int32.Parse(c.ToString()) }

let private parseLine y (line: string) =
    line.ToCharArray()
    |> Array.mapi (toPosition y)

let parse input = FileSplitter.splitInLines input |> Array.mapi parseLine
