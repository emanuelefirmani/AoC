module AdventOfCode.D10.Tests

open AdventOfCode.D10.Resolution
open TestCases
open Xunit

[<Fact>]
let ``parses map`` () =
    let actual = parse testShort
    let h = TrailHead { coordinates = { x = 3; y = 1 } }
    let s = TrailSummit { coordinates = { x = 6; y = 3 } }
    let i1 = TrailItem { coordinates = { x = 4; y = 5 }; height = 7 }
    let i2 = TrailItem { coordinates = { x = 4; y = 2 }; height = 2 }
    Assert.Equal(h, actual[1][3])
    Assert.True((trailHead (actual[5][7])).IsSome)
    Assert.Equal(s, actual[3][6])
    Assert.Equal(i1, actual[5][4])
    Assert.Equal(i2, actual[2][4])

[<Fact>]
let ``counts summits 1`` () =
    let map = parse "6690669
6661698
6662617
6543456
7651987
8761111
9871116"
    let actual = countSummits map
    
    Assert.Equal(4, actual)

[<Fact>]
let ``counts summits 2`` () =
    let map = parse "1066966
2666866
3666766
4567654
6668663
6669662
6666601"
    let actual = countSummits map
    
    Assert.Equal(3, actual)

[<Fact>]
let ``counts summits short`` () =
    let map = parse testShort
    let actual = countSummits map
    
    Assert.Equal(36, actual)

[<Fact>]
let ``counts summits long`` () =
    let map = parse testLong
    let actual = countSummits map
    
    Assert.Equal(737, actual)
    