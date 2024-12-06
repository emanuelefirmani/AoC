module AdventOfCode.D06.Tests

open Xunit
open Resolution
open InputParsing
open TestCases

[<Fact>]
let ``parses map`` () =
    let actual = parseMap testShort
    Assert.Equal(Obstacle, actual[0][4])
    Assert.Equal(Obstacle, actual[1][9])
    Assert.Equal(Obstacle, actual[8][0])
    Assert.Equal(Obstacle, actual[6][1])
    Assert.Equal(Free, actual[6][4])
    Assert.Equal(Free, actual[2][2])

[<Fact>]
let ``finds guard`` () =
    let actual = findGuard testShort
    Assert.Equal({x = 4; y = 6; direction = North }, actual)

[<Fact>]
let ``calculate middle numbers short`` () =
    let actual = calculateNumberOfPositions testShort
    Assert.Equivalent(41, actual)

[<Fact>]
let ``calculate middle numbers long`` () =
    let actual = calculateNumberOfPositions testLong
    Assert.Equivalent(5409, actual)

[<Fact>]
let ``calculates possible loops short`` () =
    let actual = calculatePossibleLoops testShort
    Assert.Equivalent(6, actual)

[<Fact(Skip = "Takes too long")>]
let ``calculates possible loops long`` () =
    let actual = calculatePossibleLoops testLong
    Assert.Equivalent(2022, actual)
