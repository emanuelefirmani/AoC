module AdventOfCode.D01.Resolution

open System
open AdventOfCode
open Microsoft.FSharp.Core
open FileSplitter

let computeItemDistance (i1: int, i2: int) =
    i1 - i2 |> Math.Abs

let computeDistance l1 l2 =
    let l1Ordered = l1 |> Seq.sort
    let l2Ordered = l2 |> Seq.sort

    let distances = Seq.zip l1Ordered l2Ordered |> Seq.map computeItemDistance

    distances |> Seq.sum

let computeDistanceFromText (text: string) =
    let numbers = splitInLinesInTwoInts text
    let l1 = splitList numbers 0
    let l2 = splitList numbers 1
    computeDistance l1 l2

let countItems<'a when 'a :> IEquatable<'a>> (list: 'a seq) (item: 'a)=
    list |> Seq.filter (fun i -> i.Equals item) |> Seq.length

let computeSimilarity (l1: int seq) (l2: int seq) =
    l1
    |> Seq.map (fun i1 -> i1 * countItems l2 i1)
    |> Seq.sum

let computeSimilarityFromText (text: string) =
    let numbers = splitInLinesInTwoInts text
    let l1 = splitList numbers 0
    let l2 = splitList numbers 1
    computeSimilarity l1 l2