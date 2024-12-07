module AdventOfCode.D07.Resolution

open System
open AdventOfCode
open FileSplitter
open Microsoft.FSharp.Core

type Operation =
    | Sum
    | Multiply
    | Concatenate

let rec getCombinationOf2Operations n : Operation list list =
    match n with
    | 1 -> [ [ Sum ]; [ Multiply ] ]
    | _ ->
        let previousCombinations = getCombinationOf2Operations (n - 1)
        let p1 = previousCombinations |> List.map (fun c -> Sum :: c)
        let p2 = previousCombinations |> List.map (fun c -> Multiply :: c)
        List.append p1 p2

let rec getCombinationOf3Operations n : Operation list list =
    match n with
    | 1 -> [ [ Sum ]; [ Multiply ]; [ Concatenate ] ]
    | _ ->
        let previousCombinations = getCombinationOf2Operations (n - 1)
        let p1 = previousCombinations |> List.map (fun c -> Sum :: c)
        let p2 = previousCombinations |> List.map (fun c -> Multiply :: c)
        let p3 = previousCombinations |> List.map (fun c -> Concatenate :: c)
        List.append p3 (List.append p1 p2)

let private applyOperation m1 m2 operation : decimal =
    match operation with
    | Sum -> m1 + m2
    | Multiply -> m1 * m2
    | Concatenate -> m1.ToString() + m2.ToString() |> decimal

let private computeEquation (members: decimal array) (operations: Operation list) =
    let mutable tot = members[0]
    for i in [0..(operations.Length - 1)] do
        tot <- applyOperation tot members[i + 1] operations[i]
    tot

let equationValidity operationsGenerator (values: decimal array) =
    let expected = values[0]
    let members = Array.skip 1 values

    let valid =
        operationsGenerator (members.Length - 1)
        |> List.map (computeEquation members)
        |> List.exists (fun result -> result = expected)
    if valid then
        Some expected
    else
        None

let computeEquationValidation operationsGenerator (values: string array) =
    match values |> Array.map decimal |> (equationValidity operationsGenerator) with
    | None -> 0m
    | Some x -> x

let computeCalibration input =
    splitInLines input
    |> Array.map (_.Split([|' '; ':'|], StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (computeEquationValidation getCombinationOf2Operations)
    |> Array.sum