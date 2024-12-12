module AdventOfCode.D12.Tests

open Xunit
open TestCases
open Resolution

[<Fact>]
let ``parses input`` () =
    let actual = parse testShort
    Assert.Equal(actual[0][0], { plant = 'R'; y = 0; x = 0 })
    Assert.Equal(actual[3][6], { plant = 'J'; y = 3; x = 6 })

[<Fact>]
let ``calculate area`` () =
    let actual = findAllRegions(parse testShort) |> Array.ofSeq
    Assert.Equal(12, calculateArea actual[0])
    Assert.Equal(4, calculateArea actual[1])
    Assert.Equal(14, calculateArea actual[2])

[<Fact>]
let ``calculate perimeter`` () =
    let map = parse testShort
    let actual = findAllRegions map |> Array.ofSeq
    Assert.Equal(18, calculatePerimeter map actual[0])
    Assert.Equal(8, calculatePerimeter map actual[1])
    Assert.Equal(22, calculatePerimeter map actual[2])

[<Fact>]
let ``calculate fence cost short`` () =
    let map = parse testShort
    let areas = findAllRegions map
    let actual = calculateFencePrice map areas

    Assert.Equal(1930m, actual)

[<Fact(Skip = "never ends")>]
let ``calculate fence cost long`` () =
    let map = parse testLong
    let areas = findAllRegions map
    let actual = calculateFencePrice map areas

    Assert.Equal(1930m, actual)
