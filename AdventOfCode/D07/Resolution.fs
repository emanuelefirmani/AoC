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

let rec getCombinations allowedOperations n : Operation list list =
    match n with
    | 1 -> allowedOperations |> List.map (fun o -> [ o ])
    | _ ->
        getCombinations allowedOperations (n - 1)
        |> List.map (fun c -> (allowedOperations |> List.map (fun o -> o :: c)))
        |> List.collect id

let private applyOperation m1 m2 operation : decimal =
    match operation with
    | Sum -> m1 + m2
    | Multiply -> m1 * m2
    | Concatenate -> m1.ToString() + m2.ToString() |> decimal

let private computeEquation (members: decimal list) (operations: Operation list) =
    let rec recursive tot (ms: decimal list) (ops: Operation list) =
        match (ms, ops) with
        | m::membersRest, op::operationsRest ->
            let newTot = applyOperation tot m op
            recursive newTot membersRest operationsRest
        | _ -> tot
    
    recursive members[0] (List.skip 1 members) operations

let equationValidity allowedOperations (values: decimal list) =
    let expected = values[0]
    let members = List.skip 1 values

    let valid =
        getCombinations allowedOperations (members.Length - 1)
        |> Seq.map (computeEquation members)
        |> Seq.exists (fun result -> result = expected)

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