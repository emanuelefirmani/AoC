module AdventOfCode.D13.Resolution

open AdventOfCode

type ButtonA = { x: int; y: int }
type ButtonB = { x: int; y: int }
type Prize = { x: int; y: int }
type ClawMachine = { buttonA: ButtonA; buttonB: ButtonB; prize: Prize }

let parseClawMachine (line: string) =
    let lines = FileSplitter.splitInLines line
    let ax = FileSplitter.readBetween lines[0] "X+" "," |> int
    let ay = FileSplitter.readTillEnd lines[0] "Y+" |> int
    let buttonA: ButtonA = { x = ax; y = ay }

    let bx = FileSplitter.readBetween lines[1] "X+" "," |> int
    let by = FileSplitter.readTillEnd lines[1] "Y+" |> int
    let buttonB: ButtonB = { x = bx; y = by }

    let px = FileSplitter.readBetween lines[2] "X=" "," |> int
    let py = FileSplitter.readTillEnd lines[2] "Y=" |> int
    let prize = { x = px; y = py }

    { buttonA = buttonA; buttonB = buttonB; prize = prize }

let parse (input: string) = input.Split("\r\n\r\n") |> Seq.map parseClawMachine |> Array.ofSeq
