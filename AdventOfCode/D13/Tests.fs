module AdventOfCode.D13.Tests

open Xunit
open TestCases
open Resolution

[<Fact>]
let ``parses input`` () =
    let actual = parse testShort 0m
    Assert.Equal(17m, actual[2].buttonA.x)
    Assert.Equal(86m, actual[2].buttonA.y)
    Assert.Equal(84m, actual[2].buttonB.x)
    Assert.Equal(37m, actual[2].buttonB.y)
    Assert.Equal(7870m, actual[2].prize.x)
    Assert.Equal(6450m, actual[2].prize.y)

[<Fact>]
let ``computes cost short`` () =
    let input = parse testShort 0m
    let actual = totalCost input
    Assert.Equal(480m, actual)

[<Fact>]
let ``computes cost long`` () =
    let input = parse testLong 10000000000000m
    let actual = totalCost input
    Assert.Equal(92871736253789m, actual)
