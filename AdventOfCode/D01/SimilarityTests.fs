module SimilarityTests

open AdventOfCode.D01
open Xunit
open Resolution

[<Fact>]
let ``similarity is computed for list`` () =
    let l1 = [3; 4; 2; 1; 3; 3]
    let l2 = [4; 3; 5; 3; 9; 3]

    let similarity = computeSimilarity l1 l2

    Assert.Equal(31, similarity)