module Tests

open AdventOfCode.D01
open Xunit
open Resolution

[<Fact>]
let ``My test`` () =
    let l1 = [3; 4; 2; 1; 3; 3]
    let l2 = [4; 3; 5; 3; 9; 3]

    let distance = computeDistance l1 l2

    Assert.Equal(11, distance)