module AdventOfCode.D08.Resolution

open System.Text.RegularExpressions
open AdventOfCode
open FileSplitter

type CoordY = int
type CoordX = int
type Coordinates = { x: CoordX; y: CoordY }
type Antenna = { x: CoordX; y: CoordY; Frequency: string }

let toCoordinates a = { x = a.x; y = a.y }

let parseLine (y: CoordY) line =
    let r = Regex("[a-zA-Z0-9]")

    r.Matches line
    |> Seq.map (fun m -> { x = m.Index; y = y; Frequency = m.Value })

let parseInput input =
    splitInLines input
    |> Seq.mapi parseLine
    |> Seq.collect id
    |> Seq.groupBy (_.Frequency)
