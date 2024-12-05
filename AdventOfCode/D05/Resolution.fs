module AdventOfCode.D05.Resolution

open AdventOfCode
open FileSplitter

type rule = { before: int; after: int }

let splitPageLine (line: string) =
    line.Split(',')
    |> Seq.map (fun v -> v |> int)
    |> List.ofSeq
let extractPageSequences (input: string) =
    input
    |> splitInLines
    |> Seq.map splitPageLine
    |> List.ofSeq

let splitRuleLine (line: string) =
    let parts = line.Split('|')
    { before = (parts[0] |> int); after = (parts[1] |> int) }
let extractRules (input: string) =
    input
    |> splitInLines
    |> Seq.map splitRuleLine
    |> List.ofSeq

let isSequenceValidForRule (sequence: int list) (rule: rule) =
    match List.tryFindIndex ((=) rule.before) sequence with
    | None -> true
    | Some v ->
        if(v = 0) then
            true
        else
            let subSequence = Seq.take v sequence |> List.ofSeq
            not(List.contains rule.after subSequence)

let isSequenceValid rules sequence =
    rules
    |> List.forall (isSequenceValidForRule sequence)
let isSequenceInvalid rules sequence =
    not(isSequenceValid rules sequence)

let readMiddleNumber (pages: int list) =
    let i = ((pages |> Seq.length) - 1) / 2
    pages[i]

let calculateSumOfCorrectMiddleNumber (input: string) =
    let chunks = input.Split("\r\n\r\n")
    let rules = extractRules (chunks[0].Trim())
    let pages = extractPageSequences (chunks[1].Trim())

    pages
    |> List.filter (isSequenceValid rules)
    |> List.map readMiddleNumber
    |> List.sum

let getPagesThatMustBeBefore rules v =
    rules
    |> List.filter (fun r -> r.after = v)
    |> List.map (_.before)

let rec reorder rules (state: int list) (sequence: int list) =
    match sequence with
    | [] -> state
    | [v] -> List.append state [v]
    | head::tail ->
        let pagesThatMustBeBefore = getPagesThatMustBeBefore rules head
        let canBeAdded =
            pagesThatMustBeBefore
            |> List.forall (fun p -> List.contains p state)
        let newState =
            if canBeAdded then
                List.append state [head]
            else
                state
        let newSequence =
            if canBeAdded then
                tail
            else
                List.append tail [head]

        reorder rules newState newSequence

let reorderByRules rules (state: int list) (sequence: int list) =
    let filteredRules =
        rules
        |> List.filter (fun r -> List.contains r.after sequence)
        |> List.filter (fun r -> List.contains r.before sequence)
    reorder filteredRules state sequence

let calculateSumOfIncorrectMiddleNumber (input: string) =
    let chunks = input.Split("\r\n\r\n")
    let rules = extractRules (chunks[0].Trim())
    let pages = extractPageSequences (chunks[1].Trim())

    pages
    |> List.filter (isSequenceInvalid rules)
    |> List.map (reorderByRules rules [])
    |> List.map readMiddleNumber
    |> List.sum
