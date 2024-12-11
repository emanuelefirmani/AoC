module AdventOfCode.D10.Tests

open AdventOfCode.D10.Resolution
open TestCases
open Xunit

[<Fact>]
let ``compact filesystem`` () =
    let actual = parse testShort
    let h = TrailHead { coordinates = { x = 2; y = 0 } }
    let s = TrailSummit { coordinates = { x = 5; y = 2 } }
    let i = TrailItem { coordinates = { x = 3; y = 4 }; height = 7 }
    Assert.Equal(h, actual[0][2])
    Assert.True(isHead (actual[4][6]))
    Assert.Equal(s, actual[2][5])
    Assert.Equal(i, actual[4][3])
