module AdventOfCode.D09.Resolution

open System

type File = { Id: int; Length: int }
type Space = { Length: int }

type Item =
    | File of File
    | Space of Space

let parseItem i c : Item =
    let fileId = i / 2
    let length = Int32.Parse(c.ToString())

    if fileId * 2 = i then
        File { Id = fileId; Length = length }
    else
        Space { Length = length }

let appendIdsToState fileId length state = List.append state (List.map (fun _ -> fileId) [ 1..length ])
let appendFileToState f state = appendIdsToState f.Id f.Length state
let appendSpaceToState s state = appendIdsToState 0 s.Length state

let rec compactFileSystem (state: int list) (items: Item list) =
    match items with
    | [] -> state
    | [ head ] ->
        match head with
        | File f -> appendFileToState f state
        | Space _ -> state
    | head :: rest ->
        match head with
        | File f ->
            let newState = appendFileToState f state
            compactFileSystem newState rest
        | Space s ->
            let mutable length = s.Length
            let mutable newState = state
            let mutable newItems = rest

            while length > 0 && newItems.Length > 0 do
                // consume last item
                let last = List.last newItems
                newItems <- List.take (newItems.Length - 1) newItems

                length <-
                    match last with
                    | Space _ -> length // discard and read next
                    | File f ->
                        if length >= f.Length then
                            // whole file fits in space
                            newState <- appendFileToState f newState
                            length - f.Length
                        else
                            // only part of the file fits -> queue back the rest of the file
                            newState <- appendIdsToState f.Id length newState
                            let consumedFile = File { f with Length = f.Length - length }
                            newItems <- List.append newItems [ consumedFile ]
                            0

            compactFileSystem newState newItems

let bigEnough (file: File) i item =
    match item with
    | File _ -> None
    | Space s -> if s.Length >= file.Length then Some(i, s.Length) else None

let rec compactFileSystemInBlocks (state: int list) (items: Item list) =
    match items with
    | [] -> state
    | [ head ] ->
        match head with
        | File f -> appendFileToState f state
        | Space s -> appendSpaceToState s state
    | head :: rest ->
        match head with
        | Space s ->
            let newState = appendSpaceToState s state
            compactFileSystemInBlocks newState rest
        | File f ->
            let targetSpace =
                rest |> List.mapi (bigEnough f) |> List.choose id |> List.tryLast

            let newRest, newState =
                match targetSpace with
                | None -> (rest, appendFileToState f state)
                | Some(i, length) ->
                    let tmpState = appendIdsToState 0 f.Length state

                    let tmpRest1 = List.take i rest
                    let tmpRest2: Item list =
                        if length > f.Length then
                            [ Space { Length = length - f.Length }; File f ]
                        else
                            [ File f ]
                    let tmpRest3 = List.skip (i + 1) rest
                    let tmpRest = List.append (List.append tmpRest1 tmpRest2) tmpRest3

                    (tmpRest, tmpState)

            compactFileSystemInBlocks newState newRest



let parse (input: string) =
    input.ToCharArray()
    |> Array.mapi parseItem
    |> List.ofArray
    |> compactFileSystem []

let compactInBlocks (input: string) =
    input.ToCharArray()
    |> Array.mapi parseItem
    |> List.ofArray
    |> List.rev
    |> compactFileSystemInBlocks []
    |> List.rev

let calculateChecksum input =
    parse input
    |> List.mapi (fun i v -> (i |> decimal) * (v |> decimal))
    |> List.sum

let calculateChecksumInBlocks input =
    compactInBlocks input
    |> List.mapi (fun i v -> (i |> decimal) * (v |> decimal))
    |> List.sum
