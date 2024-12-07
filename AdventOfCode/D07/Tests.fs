module AdventOfCode.D07.Tests

open AdventOfCode.D07.Resolution
open Xunit
open Resolution

[<Fact>]
let ``creates 2 correct combinations`` () =
    let expected =
        [
            [Sum; Sum]
            [Sum; Multiply]
            [Multiply; Sum]
            [Multiply; Multiply]
        ]
    let actual = getCombinationOfOperations 2
    Assert.Equivalent(expected, actual)

[<Fact>]
let ``creates 3 correct combinations`` () =
    let expected : Operation list list =
        [
            [Sum; Sum; Sum];
            [Sum; Sum; Multiply]
            [Sum; Multiply; Sum]
            [Sum; Multiply; Multiply]
            [Multiply; Sum; Sum]
            [Multiply; Sum; Multiply]
            [Multiply; Multiply; Sum]
            [Multiply; Multiply; Multiply]
        ]
    let actual = getCombinationOfOperations 3
    Assert.Equivalent(expected, actual)

[<Fact>]
let ``equation is correct 1`` () =
    let actual = isEquationValid [| 190m; 10m; 19m |]
    Assert.True(actual)

[<Fact>]
let ``equation is correct 2`` () =
    let actual = isEquationValid [| 3267m; 81m; 40m; 27m |]
    Assert.True(actual)

[<Fact>]
let ``equation is correct 3`` () =
    let actual = isEquationValid [| 292m; 11m; 6m; 16m; 20m |]
    Assert.True(actual)

[<Fact>]
let ``equation is not correct`` () =
    let actual = isEquationValid [| 21037m; 9m; 7m; 18m; 13m |]
    Assert.False(actual)
