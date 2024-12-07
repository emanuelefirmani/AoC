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

let Allowed2Operations = [ Sum; Multiply ]
let Allowed3Operations = [ Sum; Multiply; Concatenate ]

let private applyInverseOperation (tot: decimal) (m: decimal) =
    function
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
        let ending = m.ToString()

        if not (v.EndsWith(ending)) then
            None
        else
            Some("0" + v.Substring(0, v.Length - ending.Length) |> decimal)

let private tryInverseOperations allowedOperations (m: decimal) (tot: decimal) =
    allowedOperations
    |> List.map (applyInverseOperation tot m)
    |> List.choose id

let private tryInverseOperationsOnPossibleTotals allowedOperations (totals: decimal list) (m: decimal) =
    totals
    |> List.map (tryInverseOperations allowedOperations m)
    |> List.collect id

let rec private computeEquation allowedOperations expected (members: decimal list) =
    let tryMemberOnTotals totals item = tryInverseOperationsOnPossibleTotals allowedOperations totals item

    members
    |> List.skip 1
    |> List.rev
    |> List.fold tryMemberOnTotals [expected]
    |> List.exists ((=) members[0])

let equationValidity allowedOperations (values: decimal list) =
    let expected = values[0]
    let members = List.skip 1 values
    let valid = computeEquation allowedOperations expected members

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

let computeCalibration input = computeCalibrationByOperations input Allowed2Operations
let computeCalibrationWithConcatenation input = computeCalibrationByOperations input Allowed3Operations
