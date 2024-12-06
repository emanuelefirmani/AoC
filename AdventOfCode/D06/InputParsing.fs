module AdventOfCode.D06.InputParsing

open AdventOfCode
open FileSplitter

type Coordinates = { x: int; y: int }

type Position =
    | Free
    | Obstacle

type Direction =
    | North
    | East
    | South
    | West

type GuardPosition = { x: int; y: int; direction: Direction }
type GuardPositions =
    | Ok of Map<GuardPosition, bool>
    | Loop

let private cellToMap cell = if (cell = '#') then Obstacle else Free

let private lineToMap cells = cells |> Array.map cellToMap

let parseMap input =
    splitInLines input
    |> Array.map (_.ToCharArray())
    |> Array.map lineToMap

let private toDirection c =
    match c with
    | '<' -> West
    | '^' -> North
    | '>' -> East
    | 'v' -> South
    | _ -> failwith "wrong direction char"

let private findGuardInLine col (line: string) =
    let indexOf = line.IndexOfAny([| '<'; '^'; '>'; 'v' |])

    if (indexOf >= 0) then
        Some { x = indexOf; y = col; direction = toDirection line[indexOf] }
    else
        None

let findGuard input =
    splitInLines input
    |> Array.mapi findGuardInLine
    |> Array.choose id
    |> Array.take 1
    |> fun a -> a[0]
