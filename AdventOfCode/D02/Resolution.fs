module AdventOfCode.D02.Resolution

open System
open AdventOfCode
open FileSplitter

let isSafe report =
    let values = splitLine mapToInts report
    let differences =
        Seq.zip
            (values |> Seq.skip 1)
            (values |> Seq.take (Array.length values - 1))
            |> Seq.map (fun (v1, v2) -> v1 - v2)
    let signs = differences |> Seq.map Math.Sign

    (differences |> Seq.map Math.Abs |> Seq.forall (fun v -> v >= 1 && v <= 3))
    &&
    ((signs |> Seq.distinct |> Seq.truncate 2 |> Seq.length) = 1)

let CountSafeReports reports =
    splitInLines reports
    |> Seq.map isSafe
    |> Seq.filter id
    |> Seq.length