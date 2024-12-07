module AdventOfCode.D07.Tests

open Xunit
open TestCases
open Resolution

[<Fact>]
let ``equation is correct 1`` () =
    let actual = equationValidity Allowed2Operations [ 190m; 10m; 19m ]
    Assert.Equal(190m, actual.Value)

[<Fact>]
let ``equation is correct 1 with concatenate`` () =
    let actual = equationValidity Allowed3Operations [ 190m; 10m; 19m ]
    Assert.Equal(190m, actual.Value)

[<Fact>]
let ``equation is correct 2`` () =
    let actual = equationValidity Allowed2Operations [ 3267m; 81m; 40m; 27m ]
    Assert.Equal(3267m, actual.Value)

[<Fact>]
let ``equation is correct 2 with concatenate`` () =
    let actual = equationValidity Allowed3Operations [ 3267m; 81m; 40m; 27m ]
    Assert.Equal(3267m, actual.Value)

[<Fact>]
let ``equation is correct 3`` () =
    let actual = equationValidity Allowed2Operations [ 292m; 11m; 6m; 16m; 20m ]
    Assert.Equal(292m, actual.Value)

[<Fact>]
let ``equation is correct 3 with concatenate`` () =
    let actual = equationValidity Allowed3Operations [ 292m; 11m; 6m; 16m; 20m ]
    Assert.Equal(292m, actual.Value)

[<Fact>]
let ``equation is not correct 1`` () =
    let actual = equationValidity Allowed2Operations [ 21037m; 9m; 7m; 18m; 13m ]
    Assert.True(actual.IsNone)

[<Fact>]
let ``equation is not correct 2`` () =
    let actual = equationValidity Allowed2Operations [ 7290m; 6m; 8m; 6m; 15m ]
    Assert.True(actual.IsNone)

[<Fact>]
let ``equation is now correct with concatenation`` () =
    let actual = equationValidity Allowed3Operations [ 7290m; 6m; 8m; 6m; 15m ]
    Assert.Equal(7290m, actual.Value)

[<Fact>]
let ``computes calibration short`` () =
    let actual = computeCalibration testShort
    Assert.Equal(3749m, actual)

[<Fact>]
let ``computes calibration long`` () =
    let actual = computeCalibration testLong
    Assert.Equal(10741443549536m, actual)

[<Fact>]
let ``computes calibration with concatenation short`` () =
    let actual = computeCalibrationWithConcatenation testShort
    Assert.Equal(11387m, actual)

[<Fact>]
let ``computes calibration with concatenation long`` () =
    let actual = computeCalibrationWithConcatenation testLong
    Assert.Equal(500335179214836m, actual)
