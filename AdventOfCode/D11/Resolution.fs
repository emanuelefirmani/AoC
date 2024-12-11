module AdventOfCode.D11.Resolution

open System

type Stone = { v: decimal }

let blink (stone: Stone) : Stone seq =
    if stone.v = 0m then
        [ { v = 1m } ]
    else
        let value = stone.v.ToString()

        if value.Length % 2 = 0 then
            let v1 = value[0 .. (value.Length / 2 - 1)]
            let v2 = value[(value.Length / 2)..]
            [
                { v = Decimal.Parse(v1) }
                { v = Decimal.Parse(v2) }
            ]
        else
            [ { v = stone.v * 2024m } ]