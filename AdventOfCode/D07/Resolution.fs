module AdventOfCode.D07.Resolution

open System
open AdventOfCode
open FileSplitter
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Operation =
    | Sum
    | Multiply
    | Concatenate
let Allowed2Operations = [Sum; Multiply]
let Allowed3Operations = [Sum; Multiply; Concatenate]

let rec getCombinations allowedOperations = function
    | 1 -> allowedOperations |> List.map (fun o -> [ o ])
    | n ->
        getCombinations allowedOperations (n - 1)
        |> List.map (fun c -> (allowedOperations |> List.map (fun o -> o :: c)))
        |> List.collect id

let private applyInverseOperation (tot: decimal) (m: decimal) = function
    | Sum ->
        let result = tot - m
        if result < 0m then None else Some result
    | Multiply ->
        if m = 0m then
            None
        else
            let result = tot / m
            if result % 1m <> 0m then None else Some result
    | Concatenate ->
        let v = tot.ToString()
        if not(v.EndsWith (m.ToString())) then None else Some ("0" + v.Substring(0, v.Length - m.ToString().Length) |> decimal)

let private computeEquation expected (members: decimal list) (operations: Operation list) =
    let mutable tot = Some expected

    for i in [ (operations.Length - 1) .. -1 .. 0 ] do
        if tot.IsSome then
            tot <- (applyInverseOperation tot.Value members[i + 1] operations[i])
    
    match tot with
    | None -> false
    | Some t -> t = members[0]

let equationValidity allowedOperations (values: decimal list) =
    let expected = values[0]
    let members = List.skip 1 values

    let valid =
        getCombinations allowedOperations (members.Length - 1)
        |> Seq.map (computeEquation expected members)
        |> Seq.exists id

    if valid then Some expected else None

let private computeEquationValidation allowedOperations (values: string list) =
    match values |> List.map decimal |> (equationValidity allowedOperations) with
    | None -> 0m
    | Some x -> x

let computeCalibrationByOperations input allowedOperations =
    splitInLines input
    |> Array.map (_.Split([| ' '; ':' |], StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun l -> computeEquationValidation allowedOperations (l |> List.ofArray))
    |> Array.sum

let computeCalibration input =
    computeCalibrationByOperations input Allowed2Operations

let computeCalibrationWithConcatenation input =
    computeCalibrationByOperations input Allowed3Operations