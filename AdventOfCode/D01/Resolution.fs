module AdventOfCode.D01.Resolution

open System
open Microsoft.FSharp.Core

let computeItemDistance (i1: int, i2: int) =
    i1 - i2 |> Math.Abs

let rec computeDistance l1 l2 =
    let l1Ordered = l1 |> Seq.sort
    let l2Ordered = l2 |> Seq.sort

    let distances = Seq.zip l1Ordered l2Ordered |> Seq.map computeItemDistance

    distances |> Seq.sum
