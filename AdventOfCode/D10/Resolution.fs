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
    | Abyss

let trailHead =
    function
    | TrailHead h -> Some h
    | _ -> None

let toPosition y x c =
    let coordinates = { x = x + 1; y = y + 1 }

    match c with
    | '0' -> TrailHead { coordinates = coordinates }
    | '9' -> TrailSummit { coordinates = coordinates }
    | _ -> TrailItem { coordinates = coordinates; height = Int32.Parse(c.ToString()) }

let applyToNeighbours f y x =
    Seq.concat [
        f (y - 1) x
        f y (x - 1)
        f (y + 1) x
        f y (x + 1)
    ]

let rec private letsHike (map: Position array array) (trailItem: TrailItem) : TrailSummit seq =
    let f y x =
        match map[y][x] with
        | TrailSummit s ->
            if(trailItem.height = 8) then
                [s]
            else
                []
        | TrailItem i ->
            if(trailItem.height + 1 = i.height) then
                letsHike map i |> List.ofSeq
            else
                []
        | _ -> []

    applyToNeighbours f trailItem.coordinates.y trailItem.coordinates.x

let rec private letsStartHiking (map: Position array array) (item: TrailHead) : TrailSummit seq =
    let f y x : TrailSummit seq =
        match map[y][x] with
        | TrailItem i ->
            if i.height = 1 then
                letsHike map i
            else
                []
        | _ -> []

    applyToNeighbours f item.coordinates.y item.coordinates.x

let countSummitPerHead map head =
    letsStartHiking map head
    |> Seq.length

let countDistinctSummitPerHead map head =
    letsStartHiking map head
    |> Seq.distinct
    |> Seq.length

let countDistinctSummits map =
    map
    |> Seq.collect id
    |> Seq.map trailHead
    |> Seq.choose id
    |> Seq.map (countDistinctSummitPerHead map)
    |> Seq.sum

let countSummits map =
    map
    |> Seq.collect id
    |> Seq.map trailHead
    |> Seq.choose id
    |> Seq.map (countSummitPerHead map)
    |> Seq.sum

let private parseLine y (line: string) =
    let items = line.ToCharArray() |> Array.mapi (toPosition y)
    Array.concat [ [| Abyss |]; items; [| Abyss |] ]

let parse input =
    let items = FileSplitter.splitInLines input |> Array.mapi parseLine

    let abysses =
        [|[ 1 .. items[0].Length ] |> List.map (fun _ -> Abyss) |> Array.ofSeq |]

    Array.concat [abysses; items; abysses]
