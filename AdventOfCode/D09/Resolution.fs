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

let rec expandList (state: int list) (items: Item list) =
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
            expandList newState rest
        | Space s ->
            let mutable length = s.Length
            let mutable newState = state
            let mutable newItems = rest

            while length > 0 && newItems.Length > 0 do
                // consume last item
                let last = List.last newItems
                newItems <- List.removeAt (newItems.Length - 1) newItems

                length <-
                    match last with
                    | Space _ -> length // discard and read next
                    | File f ->
                        if s.Length >= f.Length then
                            // whole file fits in space
                            newState <- appendFileToState f newState
                            length - f.Length
                        else
                            // only part of the file fits -> queue back the rest of the file
                            newState <- appendIdsToState f.Id length newState
                            let consumedFile = File { f with Length = f.Length - length }
                            newItems <- List.append newItems [ consumedFile ]
                            0

            expandList newState newItems

let parse (input: string) =
    input.ToCharArray()
    |> Array.mapi parseItem
    |> List.ofArray
    |> expandList []

let calculateChecksum input =
    parse input
    |> List.mapi (fun i v -> (i |> decimal) * (v |> decimal))
    |> List.sum