module AdventOfCode.D13.Resolution

open System
open AdventOfCode
open Microsoft.FSharp.Core

type ButtonA = { x: decimal; y: decimal }
type ButtonB = { x: decimal; y: decimal }
type Prize = { x: decimal; y: decimal }
type ClawMachine = { buttonA: ButtonA; buttonB: ButtonB; prize: Prize }

type MatchingCombination =
    { clawMachine: ClawMachine
      repetitionA: decimal
      repetitionB: decimal
      cost: decimal }

let parseClawMachine offset (line: string) =
    let lines = FileSplitter.splitInLines line
    let ax = FileSplitter.readBetween lines[0] "X+" "," |> decimal
    let ay = FileSplitter.readTillEnd lines[0] "Y+" |> decimal
    let buttonA: ButtonA = { x = ax; y = ay }

    let bx = FileSplitter.readBetween lines[1] "X+" "," |> decimal
    let by = FileSplitter.readTillEnd lines[1] "Y+" |> decimal
    let buttonB: ButtonB = { x = bx; y = by }

    let px = offset + (FileSplitter.readBetween lines[2] "X=" "," |> decimal)
    let py = offset + (FileSplitter.readTillEnd lines[2] "Y=" |> decimal)
    let prize = { x = px; y = py }

    { buttonA = buttonA; buttonB = buttonB; prize = prize }

let parse (input: string) offset =
    input.Split("\r\n\r\n")
    |> Seq.map (parseClawMachine offset)
    |> Array.ofSeq

let cost (clawMachine: ClawMachine) =
    let repetitionA =
        Math.Floor(
            (clawMachine.prize.x * clawMachine.buttonB.y - clawMachine.prize.y * clawMachine.buttonB.x) /
            (clawMachine.buttonA.x * clawMachine.buttonB.y - clawMachine.buttonA.y * clawMachine.buttonB.x)
            )
                      
    let repetitionB =
        Math.Floor(
            (clawMachine.prize.x - repetitionA * clawMachine.buttonA.x) / clawMachine.buttonB.x) 

    if repetitionB * clawMachine.buttonB.y + repetitionA * clawMachine.buttonA.y <> clawMachine.prize.y ||
       repetitionB * clawMachine.buttonB.x + repetitionA * clawMachine.buttonA.x <> clawMachine.prize.x then
        None
    else
        Some
            { clawMachine = clawMachine
              repetitionA = repetitionA
              repetitionB = repetitionB
              cost = repetitionA * 3m + repetitionB }

let totalCost clawMachines =
    clawMachines
    |> Seq.map cost
    |> Seq.choose id
    |> Seq.map _.cost
    |> Seq.sum