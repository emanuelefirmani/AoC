﻿module AdventOfCode.D07.Resolution

open Microsoft.FSharp.Core

type Operation =
    | Sum
    | Multiply

let rec getCombinationOfOperations n : Operation list list =
    match n with
    | 1 -> [ [ Sum ]; [ Multiply ] ]
    | _ ->
        let previousCombinations = getCombinationOfOperations (n - 1)
        let p1 = previousCombinations |> List.map (fun c -> Sum :: c)
        let p2 = previousCombinations |> List.map (fun c -> Multiply :: c)
        List.append p1 p2

let private applyOperation m1 m2 operation : decimal =
    match operation with
    | Sum -> m1 + m2
    | Multiply -> m1 * m2

let private computeEquation (members: decimal array) (operations: Operation list) =
    let mutable tot = members[0]
    for i in [0..(operations.Length - 1)] do
        tot <- applyOperation tot members[i + 1] operations[i]
    tot

let isEquationValid (values: decimal array) =
    let expected = values[0]
    let members = Array.skip 1 values

    getCombinationOfOperations (members.Length - 1)
    |> List.map (computeEquation members)
    |> List.exists (fun result -> result = expected)
