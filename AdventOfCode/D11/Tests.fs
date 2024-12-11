module AdventOfCode.D11.Tests

open System.Collections.Generic
open Xunit
open Resolution
open TestCases

[<Fact>]
let ``goes to 1`` () =
    let s = { v = 0m; loop = 17 }
    let actual = blink s |> Array.ofSeq
    Assert.Equal(1m, actual[0].v)
    Assert.Equal(18, actual[0].loop)

[<Fact>]
let ``splits`` () =
    let s = { v = 1241m; loop = 21 }
    let actual = blink s |> Array.ofSeq
    Assert.Equal(12m, actual[0].v)
    Assert.Equal(22, actual[0].loop)
    Assert.Equal(41m, actual[1].v)
    Assert.Equal(22, actual[1].loop)

[<Fact>]
let ``multiplies`` () =
    let s = { v = 100m; loop = 5 }
    let actual = blink s |> Array.ofSeq
    Assert.Equal(202400m, actual[0].v)
    Assert.Equal(6, actual[0].loop)

[<Fact>]
let ``after blinking`` () =
    let input = parse testShort
    let actual1 = afterBlinking (Dictionary<Stone, decimal>()) 1 input
    Assert.Equivalent(3, actual1)

    let actual6 = afterBlinking (Dictionary<Stone, decimal>()) 6 input
    Assert.Equivalent(22, actual6)

[<Fact>]
let ``count stones short`` () =
    let input = parse testShort
    let actual = afterBlinking (Dictionary<Stone, decimal>()) 25 input
    Assert.Equivalent(55312m, actual)

[<Fact>]
let ``count stones long`` () =
    let input = parse testLong
    let actual = afterBlinking (Dictionary<Stone, decimal>()) 25 input
    Assert.Equivalent(217812m, actual)

[<Fact()>]
let ``count stones long part two`` () =
    let input = parse testLong
    let actual = afterBlinking (Dictionary<Stone, decimal>()) 75 input
    Assert.Equivalent(259112729857522m, actual)
