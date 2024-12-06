module AdventOfCode.D06.Resolution

open AdventOfCode.D06.InputParsing

let private toCoordinate guard = { x = guard.x; y = guard.y }

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
        | Obstacle -> Some (rotateGuard guard)

let rec private moveGuardAcrossPositions map guard positions =
    match moveGuard map guard with
    | None -> Ok positions
    | Some next ->
        if List.contains next positions then
            Loop
        else
            moveGuardAcrossPositions map next (next :: positions)

let private analyze input =
    let guard = findGuard input
    let map = parseMap input
    moveGuardAcrossPositions map guard [ guard ]

let getPositions guardPositions =
    match guardPositions with
    | Loop -> failwith "Ended in loop"
    | Ok positions -> positions

let isLoop guardPositions =
    match guardPositions with
    | Loop -> true
    | Ok _ -> false

let calculateNumberOfPositions input =
    analyze input
    |> getPositions
    |> List.map toCoordinate
    |> List.distinct
    |> List.length

let tryLoop initialGuard (map: Position array array) (coordinates: Coordinates) =
    if map[coordinates.y][coordinates.x] = Obstacle then
        None
    else if toCoordinate initialGuard = coordinates then
        None
    else
        let newMap =
            map
            |> Array.map (fun l -> l |> Array.map id)
        newMap[coordinates.y][coordinates.x] <- Obstacle
        let result = moveGuardAcrossPositions newMap initialGuard [ initialGuard ]
        if result = Loop then
            Some coordinates
        else
            None

let calculatePossibleLoops input =
    let map = parseMap input
    let initialGuard = findGuard input

    analyze input
    |> getPositions
    |> List.map toCoordinate
    |> List.distinct
    |> List.map (tryLoop initialGuard map)
    |> List.choose id
    |> List.distinct
    |> List.length