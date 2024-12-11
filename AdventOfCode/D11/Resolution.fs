module AdventOfCode.D11.Resolution

open System
open System.Collections.Generic
open Microsoft.FSharp.Core

type Stone = { v: decimal; loop: int }

let blink (stone: Stone) =
    if stone.v = 0m then
        [{ v = 1m; loop = stone.loop + 1 }]
    else
        let value = stone.v.ToString()

        if value.Length % 2 = 0 then
            let v1 = value[0 .. (value.Length / 2 - 1)]
            let v2 = value[(value.Length / 2) ..]
            [
             { v = Decimal.Parse(v1); loop = stone.loop + 1 }
             { v = Decimal.Parse(v2); loop = stone.loop + 1 }
             ]
        else
            [{ v = stone.v * 2024m; loop = stone.loop + 1 }]

let rec afterBlinking (dic: Dictionary<Stone, decimal>) n (stones: Stone seq) =
    match n with
    | 0 -> stones |> Seq.length |> decimal
    | n ->
        stones
        |> Seq.map (fun s ->
            if dic.ContainsKey(s) then
                dic[s]
            else
                let total = blink s |> afterBlinking dic (n - 1)
                dic.Add(s, total)
                total
            )
        |> Seq.sum

let parse (input: string) =
    input.Split(' ')
    |> Seq.map decimal
    |> Seq.map (fun x -> { v = x; loop = 0 })
