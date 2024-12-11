module AdventOfCode.D11.Tests

open Xunit
open Resolution

[<Fact>]
let ``goes to 1`` () =
    let s = { v = 0m }
    let actual = blink s |> Array.ofSeq
    Assert.Equal(1m, actual[0].v)

[<Fact>]
let ``splits`` () =
    let s = { v = 1241m }
    let actual = blink s |> Array.ofSeq
    Assert.Equal(12m, actual[0].v)
    Assert.Equal(41m, actual[1].v)

[<Fact>]
let ``multiplies`` () =
    let s = { v = 100m }
    let actual = blink s |> Array.ofSeq
    Assert.Equal(202400m, actual[0].v)