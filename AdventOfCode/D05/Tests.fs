module AdventOfCode.D05.Tests

open Xunit
open Resolution
open TestCases

[<Fact>]
let ``calculate middle numbers short`` () =
    let actual = calculateSumOfCorrectMiddleNumber testShort
    Assert.Equivalent(143, actual)

[<Fact>]
let ``calculate middle numbers long`` () =
    let actual = calculateSumOfCorrectMiddleNumber testLong
    Assert.Equivalent(6051, actual)

[<Fact>]
let ``calculate middle numbers of incorrect short`` () =
    let actual = calculateSumOfIncorrectMiddleNumber testShort
    Assert.Equivalent(123, actual)
    
[<Fact>]
let ``calculate middle numbers of incorrect full`` () =
    let actual = calculateSumOfIncorrectMiddleNumber testLong
    Assert.Equivalent(5093, actual)