﻿module AdventOfCode.D08.Resolution

open System
open System.Text.RegularExpressions
open AdventOfCode
open FileSplitter

type CoordY = int
type CoordX = int
type Coordinates = { x: CoordX; y: CoordY }
type Antenna = { x: CoordX; y: CoordY; Frequency: string }

let toCoordinates a = { x = a.x; y = a.y }

let private antinodesOfAntennas repetitions (a1: Antenna, a2: Antenna) =
    repetitions a1 a2
    |> Seq.map (fun i -> {
        x = a2.x - i * (a1.x - a2.x)
        y = a2.y - i * (a1.y - a2.y)
    })

let frequencyAntinodeCoordinates repetitions (antennas: Antenna seq) =
    query {
        for a1 in antennas do
        for a2 in antennas do
        where (a1 <> a2)
        select (a1, a2)
    }
    |> Seq.map (antinodesOfAntennas repetitions)
    |> Seq.collect id

let parseLine (y: CoordY) line =
    Regex("[a-zA-Z0-9]").Matches line
    |> Seq.map (fun m -> { x = m.Index; y = y; Frequency = m.Value })

let parseInput input =
    splitInLines input
    |> Seq.mapi parseLine
    |> Seq.collect id
    |> Seq.groupBy (_.Frequency)

let countAntinodes input listRepetitions =
    let lines = splitInLines input
    let mapCorner = { x = (Seq.head lines).Length - 1; y = lines.Length - 1 }
    let antennas = parseInput input
    let repetitions = listRepetitions mapCorner
    
    antennas
    |> Seq.map snd
    |> Seq.map (frequencyAntinodeCoordinates repetitions)
    |> Seq.collect id
    |> Seq.distinct
    |> Seq.length

let maxRepetitions (mapCorner: Coordinates) (a1: Antenna) (a2: Antenna) =
    let repetitions max (v1: int) (v2: int) =
        if v1 = v2 then
            Int32.MaxValue
        else if v1 > v2 then
            v2 / (v1 - v2)
        else
            (max - v2) / (v2 - v1)

    Math.Min(repetitions mapCorner.x a1.x a2.x, repetitions mapCorner.y a1.y a2.y)

let countAntinodesWithoutRepetitions input =
    let repetitions c a1 a2 = [1..Math.Min(1, (maxRepetitions c a1 a2))]
    countAntinodes input repetitions

let countAntinodesWithRepetitions input =
    let repetitions c a1 a2 = [0..(maxRepetitions c a1 a2)]

    countAntinodes input repetitions
