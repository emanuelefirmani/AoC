module AdventOfCode.D08.Tests

open AdventOfCode.D08.Resolution
open Xunit
open TestCases

let antennaCoordinates input frequency =
    let actual = parseInput input

    actual
    |> Seq.filter (fun (f, _) -> f = frequency)
    |> Seq.map snd
    |> Seq.exactlyOne
    |> Seq.map toCoordinates
    |> List.ofSeq

[<Fact>]
let ``parses map for antennas 0`` () =
    let expected = [
        { x = 8; y = 1 }
        { x = 5; y = 2 }
        { x = 7; y = 3 }
        { x = 4; y = 4 }
    ]
    
    let actual = antennaCoordinates testShort "0"

    Assert.Equivalent(expected, actual)
    Assert.Equal(expected.Length, actual.Length)

[<Fact>]
let ``parses map for antennas A`` () =
    let expected = [
        { x = 6; y = 5 }
        { x = 8; y = 8 }
        { x = 9; y = 9 }
    ]
    
    let actual = antennaCoordinates testShort "A"

    Assert.Equivalent(expected, actual)
    Assert.Equal(expected.Length, actual.Length)

[<Fact>]
let ``counts antinodes short`` () =
    let actual = countAntinodesWithoutRepetitions testShort
    Assert.Equal(14, actual)

[<Fact>]
let ``counts antinodes long`` () =
    let actual = countAntinodesWithoutRepetitions testLong
    Assert.Equal(392, actual)

[<Fact>]
let ``counts antinodes with repetitions short`` () =
    let actual = countAntinodesWithRepetitions testShort
    Assert.Equal(34, actual)

[<Fact>]
let ``counts antinodes with repetitions long`` () =
    let actual = countAntinodesWithRepetitions testLong
    Assert.Equal(1235, actual)
