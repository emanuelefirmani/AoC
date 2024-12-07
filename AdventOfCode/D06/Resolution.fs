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

let rec private moveGuardAcrossPositions map guard (positions: VisitedPositions) =
    match moveGuard map guard with
    | None -> Ok positions
    | Some next ->
        if positions.Contains next  then
            Loop
        else
            moveGuardAcrossPositions map next (positions.Add(next))

let private getPositions guardPositions =
    match guardPositions with
    | Loop -> failwith "Ended in loop"
    | Ok positions -> List.ofSeq positions

let private isLoop guardPositions =
    match guardPositions with
    | Loop -> true
    | Ok _ -> false

let private moveGuardAcrossMap map guard =
    let initialPositions : VisitedPositions = VisitedPositions [ guard ]
    moveGuardAcrossPositions map guard initialPositions

let private analyze guard map =
    moveGuardAcrossMap map guard
    |> getPositions
    |> List.map toCoordinate
    |> List.distinct

let calculateNumberOfPositions input =
    let parsed = parse input
    analyze parsed.guard parsed.map
    |> List.length

let private tryLoop initialGuard (map: Position array array) (coordinates: Coordinates) =
    if map[coordinates.y][coordinates.x] = Obstacle then
        None
    else if toCoordinate initialGuard = coordinates then
        None
    else
        let newMap =
            map
            |> Array.map (fun l -> l |> Array.map id)
        newMap[coordinates.y][coordinates.x] <- Obstacle
        let result = moveGuardAcrossMap newMap initialGuard
        if result = Loop then
            Some coordinates
        else
            None

let calculatePossibleLoops input =
    let parsed = parse input

    analyze parsed.guard parsed.map
    |> List.map (tryLoop parsed.guard parsed.map)
    |> List.choose id
    |> List.distinct
    |> List.length