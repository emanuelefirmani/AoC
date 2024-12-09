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
let ``expands list`` () =
    let actual = parseToString "12345"
    Assert.Equal("022111222", actual)

[<Fact>]
let ``expands list with exact file-to-space match`` () =
    let actual = parseToString "1533515"
    Assert.Equal("03333311122222", actual)

[<Fact>]
let ``expands list 2`` () =
    let actual = parseToString testShort
    Assert.Equal("0099811188827773336446555566", actual)

[<Fact>]
let ``compute checksum short`` () =
    let actual = calculateChecksum testShort
    
    Assert.Equal(1928m, actual)

[<Fact>]
let ``compute checksum long`` () =
    let actual = calculateChecksum testLong
    
    Assert.Equal(6186204235825m, actual)