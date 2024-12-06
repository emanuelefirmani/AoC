module AdventOfCode.D06.Resolution

open AdventOfCode
open FileSplitter

type Position =
    | Free
    | Obstacle

type Direction =
    | North
    | East
    | South
    | West

type GuardPosition = { x: int; y: int; direction: Direction }

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

let private nextGuardPosition (guard: GuardPosition) =
    match guard.direction with
    | North -> { guard with y = guard.y - 1 }
    | East -> { guard with x = guard.x + 1 }
    | South -> { guard with y = guard.y + 1 }
    | West -> { guard with x = guard.x - 1 }

let private isGuardOutsideOfMap (map: Position array array) (guard: GuardPosition) =
    guard.x < 0
    || guard.y < 0
    || guard.x >= map[0].Length
    || guard.y >= map.Length

let private rotateGuard (guard: GuardPosition) =
    match guard.direction with
    | North -> { guard with direction = East }
    | East -> { guard with direction = South }
    | South -> { guard with direction = West }
    | West -> { guard with direction = North }

let rec private moveGuard map guard =
    let next = nextGuardPosition guard

    if isGuardOutsideOfMap map next then
        None
    else
        match map[next.y][next.x] with
        | Free -> Some next
        | Obstacle -> moveGuard map (rotateGuard guard)

let rec private moveGuardAcrossPositions map guard positions =
    match moveGuard map guard with
    | None -> positions
    | Some next -> moveGuardAcrossPositions map next (next :: positions)

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

let calculateNumberOfPositions input =
    let guard = findGuard input
    let map = parseMap input

    let moveGuardAcrossPositions =
        moveGuardAcrossPositions map guard [ guard ]

    moveGuardAcrossPositions
    |> List.map (fun p -> {| x = p.x; y = p.y |})
    |> List.distinct
    |> List.length
