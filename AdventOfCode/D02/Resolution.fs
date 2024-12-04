module AdventOfCode.D02.Resolution

open System
open AdventOfCode
open FileSplitter

let private computeDifferences (values: int seq) =
    Seq.zip
        (values |> Seq.skip 1)
        (values |> Seq.take (Seq.length values - 1))
        |> Seq.map (fun (v1, v2) -> v1 - v2)

let private computeSigns (differences: int seq) = differences |> Seq.map Math.Sign

let private areSafe (differences: int seq) signs = 
    (differences |> Seq.map Math.Abs |> Seq.forall (fun v -> v >= 1 && v <= 3))
    &&
    ((signs |> Seq.distinct |> Seq.truncate 2 |> Seq.length) = 1)

let private areValuesSafe (values: int seq) =
    let differences = computeDifferences values
    let signs = computeSigns differences
    areSafe differences signs

let isSafe report =
    let values = splitLine mapToInts report
    areValuesSafe values

let countSafeReports reports =
    splitInLines reports
    |> Seq.map isSafe
    |> Seq.filter id
    |> Seq.length

let rec private remove i (l: int list) =
    match i, l with
    | 0, _::xs -> xs
    | i, x::xs -> x::remove (i - 1) xs
    | _, [] -> failwith "index out of range"

let compareSubList (l: int array) index =
    let subList =
        l
        |> List.ofSeq
        |> remove index
        |> Seq.ofList
    areValuesSafe subList

let isDampenedSafe report =
    let values = splitLine mapToInts report
    let areSafe = areValuesSafe values
    if areSafe then
        true
    else
        [0..(-1 + Seq.length values)]
        |> Seq.exists (compareSubList values)

let countDampenedSafeReports reports =
    splitInLines reports
    |> Seq.map isDampenedSafe
    |> Seq.filter id
    |> Seq.length
