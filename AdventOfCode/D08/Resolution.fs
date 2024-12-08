module AdventOfCode.D08.Resolution

open System.Text.RegularExpressions
open AdventOfCode
open FileSplitter

type CoordY = int
type CoordX = int
type Coordinates = { x: CoordX; y: CoordY }
type Antenna = { x: CoordX; y: CoordY; Frequency: string }

let toCoordinates a = { x = a.x; y = a.y }

let private isWithinMap (mapCorner: Coordinates) (c: Coordinates) =
    c.x >= 0
    && c.y >= 0
    && c.x <= mapCorner.x
    && c.y <= mapCorner.y

let antinodeCoordinate (a1: Antenna, a2: Antenna) =
    { x = 2 * a2.x - a1.x; y = 2 * a2.y - a1.y}

let frequencyAntinodeCoordinates (antennas: Antenna seq) =
    query {
        for a1 in antennas do
        for a2 in antennas do
        where (a1 <> a2)
        select (a1, a2)
    }
    |> Seq.map antinodeCoordinate

let parseLine (y: CoordY) line =
    let r = Regex("[a-zA-Z0-9]")

    r.Matches line
    |> Seq.map (fun m -> { x = m.Index; y = y; Frequency = m.Value })

let parseInput input =
    splitInLines input
    |> Seq.mapi parseLine
    |> Seq.collect id
    |> Seq.groupBy (_.Frequency)

let countAntinodes input =
    let lines = splitInLines input
    let mapCorner = { x = (Seq.head lines).Length - 1; y = lines.Length - 1 }
    let antennas = parseInput input

    antennas
    |> Seq.map snd
    |> Seq.map frequencyAntinodeCoordinates
    |> Seq.collect id
    |> Seq.filter (isWithinMap mapCorner)
    |> Seq.distinct
    |> Seq.length
