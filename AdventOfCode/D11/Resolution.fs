module AdventOfCode.D11.Resolution

open System

type Stone = { v: decimal; loops: int }

let blink (stone: Stone) (queue: Stone list) =
    if stone.v = 0m then
        ({ v = 1m; loops = stone.loops + 1 }, queue)
    else
        let value = stone.v.ToString()

        if value.Length % 2 = 0 then
            let v1 = value[0 .. (value.Length / 2 - 1)]
            let v2 = value[(value.Length / 2) ..]
            (
                { v = Decimal.Parse(v1); loops = stone.loops + 1 },
                { v = Decimal.Parse(v2); loops = stone.loops + 1 } :: queue
            )
        else
            ({ v = stone.v * 2024m; loops = stone.loops + 1 }, queue)

let rec blinkN n (stone: Stone) (queue: Stone list) =
    if stone.loops = n then
        queue
    else
        let newStone, newQueue = blink stone queue
        blinkN n newStone newQueue

let afterBlinking n (queue: Stone list) =
    let mutable i = 0
    let mutable newQueue = queue
    while (newQueue.Length > 0) do
        let head = List.head newQueue
        let tail =
            if newQueue.Length > 1 then
                List.tail newQueue
            else
                []
        newQueue <- blinkN n head tail
        i <- i + 1
    i

let parse (input: string) =
    input.Split(' ')
    |> Seq.map decimal
    |> Seq.map (fun x -> { v = x; loops = 0 })
    |> List.ofSeq
