module AdventOfCode.D13.Tests

open Xunit
open TestCases
open Resolution

[<Fact>]
let ``parses input`` () =
    let actual = parse testShort
    Assert.Equal(17, actual[2].buttonA.x)
    Assert.Equal(86, actual[2].buttonA.y)
    Assert.Equal(84, actual[2].buttonB.x)
    Assert.Equal(37, actual[2].buttonB.y)
    Assert.Equal(7870, actual[2].prize.x)
    Assert.Equal(6450, actual[2].prize.y)

[<Fact>]
let ``computes cost short`` () =
    let input = parse testShort
    let actual = totalCost input
    Assert.Equal(480, actual)

[<Fact>]
let ``computes cost long`` () =
    let input = parse testLong
    let actual = totalCost input
    Assert.Equal(480, actual)
