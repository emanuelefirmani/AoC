module AdventOfCode.D03.Resolution

open System.Text.RegularExpressions

let parseAndMultiply v1 v2 =
    let d1 = v1 |> decimal
    let d2 = v2 |> decimal
    d1 * d2

let computeMultiplications input =
    Regex.Matches(input, "mul\((\d*),(\d*)\)")
    |> Seq.map (fun m -> parseAndMultiply m.Groups[1].Value m.Groups[2].Value)
    |> Seq.sum