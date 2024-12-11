module AdventOfCode.D11.Tests

open Xunit
open Resolution
open TestCases

[<Fact>]
let ``goes to 1`` () =
    let s = { v = 0m; loops = 41 }
    let a, _ = blink s []
    Assert.Equal(1m, a.v)
    Assert.Equal(42, a.loops)

[<Fact>]
let ``splits`` () =
    let s = { v = 1241m; loops = 17 }
    let a, q = blink s []
    Assert.Equal(12m, a.v)
    Assert.Equal(41m, q.Head.v)
    Assert.Equal(18m, q.Head.loops)

[<Fact>]
let ``multiplies`` () =
    let s = { v = 100m; loops = 12 }
    let a, _ = blink s []
    Assert.Equal(202400m, a.v)

[<Fact>]
let ``after blinking`` () =
    let input = parse testShort
    let actual1 = afterBlinking 1 input
    Assert.Equivalent(3, actual1)

    let actual6 = afterBlinking 6 input
    Assert.Equivalent(22, actual6)

[<Fact>]
let ``count stones short`` () =
    let input = parse testShort
    let actual = afterBlinking 25 input
    Assert.Equivalent(55312m, actual)

[<Fact>]
let ``count stones long`` () =
    let input = parse testLong
    let actual = afterBlinking 25 input
    Assert.Equivalent(217812m, actual)

[<Fact>]
let ``count stones long part two`` () =
    let input = parse testLong
    let actual = afterBlinking 75 input
    Assert.Equivalent(55312m, actual)
