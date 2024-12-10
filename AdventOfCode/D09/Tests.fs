module AdventOfCode.D09.Tests

open AdventOfCode.D09.Resolution
open Xunit
open System
open TestCases

let parseToString input =
    let array =
        parse input
        |> List.map _.ToString()
    String.Join("", array)

[<Fact>]
let ``compact filesystem`` () =
    let actual = parseToString "12345"
    Assert.Equal("022111222", actual)

[<Fact>]
let ``compact filesystem with exact file-to-space match`` () =
    let actual = parseToString "1533515"
    Assert.Equal("03333311122222", actual)

[<Fact>]
let ``compact filesystem 2`` () =
    let actual = parseToString testShort
    Assert.Equal("0099811188827773336446555566", actual)

[<Fact>]
let ``compact filesystem 3`` () =
    let actual = parseToString "24854985253541181957739287987372996558882497114196891429411"
    Assert.Equal("002928282811111111282727262522222525252525252524243333333324242424244424242323235552322212121666621721212121212020198191919191919191818999991818181717171710101010101010171716111111111111111111161612121212121212121616161616161513131313131313131315151515151514141414141414", actual)

[<Fact>]
let ``compute checksum above 9`` () =
    let actual = calculateChecksum "1010101010101010101010"
    Assert.Equal(385m, actual)
    
[<Fact>]
let ``compute checksum above 9 bis`` () =
    let actual = calculateChecksum "10101010101010101010101"
    Assert.Equal(506m, actual)

[<Fact>]
let ``compute checksum above 9 ter`` () =
    let actual = calculateChecksum "111111111111111111111"
    Assert.Equal(290m, actual)

[<Fact>]
let ``compute checksum short`` () =
    let actual = calculateChecksum testShort
    Assert.Equal(1928m, actual)

[<Fact>]
let ``compute checksum long`` () =
    let actual = calculateChecksum testLong
    Assert.Equal(6330095022244m, actual)

[<Fact>]
let ``compact filesystem in blocks`` () =
    let actual = compactInBlocks testShort
    Assert.Equal("009921117770440333000055550666600000888800", String.Join("", actual))

[<Fact>]
let ``compute checksum in blocks short`` () =
    let actual = calculateChecksumInBlocks testShort
    Assert.Equal(2858m, actual)

[<Fact>]
let ``compute checksum in blocks long`` () =
    let actual = calculateChecksumInBlocks testLong
    Assert.Equal(6359491814941m, actual)
