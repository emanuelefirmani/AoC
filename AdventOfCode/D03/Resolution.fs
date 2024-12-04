module AdventOfCode.D03.Resolution

open System
open System.Text.RegularExpressions

let private startTag = "do()"
let private endTag = "don't()"

let parseAndMultiply v1 v2 =
    let d1 = v1 |> decimal
    let d2 = v2 |> decimal
    d1 * d2

let computeMultiplications input =
    Regex.Matches(input, "mul\\((\\d*),(\\d*)\\)")
    |> Seq.map (fun m -> parseAndMultiply m.Groups[1].Value m.Groups[2].Value)
    |> Seq.sum

let trimDont (input: string) =
    let indexOf = input.IndexOf(endTag)
    if indexOf < 0 then
        input
    else
        input.Substring(0, indexOf)

let computeMultiplicationsAfterDos (input: string) =
    let firstStart = if input.IndexOf(startTag) < 0 then Int32.MaxValue else input.IndexOf(startTag) 
    let firstEnd = if input.IndexOf(endTag) < 0 then Int32.MaxValue else input.IndexOf(endTag) 
    let firstPart = input.Substring(0, Math.Min(firstStart, firstEnd))
    let s1 = computeMultiplications firstPart

    let s2 =    
        input
            .Substring(Math.Min(firstStart, firstEnd))
            .Split(startTag)
            |> Seq.map trimDont
            |> Seq.map computeMultiplications
            |> Seq.sum

    s1 + s2