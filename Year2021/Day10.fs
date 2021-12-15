// Advent of Code Year 2021
// --- Day 10: Syntax Scoring ---
// https://adventofcode.com/2021/day/10
module Year2021.Day10

type SyntaxError =
    | Corrupted of char
    | Incomplete of char list

let rec processSyntax (stack: char list) (line: char list) =
    match (line, stack) with
    | '(' :: rest, _ -> processSyntax ('(' :: stack) rest
    | '[' :: rest, _ -> processSyntax ('[' :: stack) rest
    | '{' :: rest, _ -> processSyntax ('{' :: stack) rest
    | '<' :: rest, _ -> processSyntax ('<' :: stack) rest
    | ')' :: rest, '(' :: stack -> processSyntax stack rest
    | '}' :: rest, '{' :: stack -> processSyntax stack rest
    | ']' :: rest, '[' :: stack -> processSyntax stack rest
    | '>' :: rest, '<' :: stack -> processSyntax stack rest
    | closing :: _, _ -> Corrupted closing
    | [], stack -> Incomplete stack

let collectSyntaxError f =
    List.map (processSyntax [])
    >> List.map f
    >> List.choose id

let part_1 =
    let corruptedData =
        collectSyntaxError
            (function
            | Corrupted data -> Some data
            | _ -> None)

    let syntaxErrorScore (data: char list) =
        let rec calculate (data: char list) (score: int) =
            match data with
            | ')' :: rest -> rest |> calculate <| score + 3
            | ']' :: rest -> rest |> calculate <| score + 57
            | '}' :: rest -> rest |> calculate <| score + 1197
            | '>' :: rest -> rest |> calculate <| score + 25137
            | _ -> score

        calculate data 0

    corruptedData >> syntaxErrorScore

let part_2 =
    let incompleteData =
        collectSyntaxError
            (function
            | Incomplete data -> Some data
            | _ -> None)

    let completionScore (data: char list list) =
        let completionScoreLine (multiplier: int64) (data: char list) =
            let rec calculate (data: char list) (score: int64) =
                match data with
                | '(' :: rest -> rest |> calculate <| score * multiplier + 1L
                | '[' :: rest -> rest |> calculate <| score * multiplier + 2L
                | '{' :: rest -> rest |> calculate <| score * multiplier + 3L
                | '<' :: rest -> rest |> calculate <| score * multiplier + 4L
                | _ -> score

            calculate data 0

        List.map (completionScoreLine 5L) data

    let winner (data: int64 list) =
        data
        |> List.sort
        |> Array.ofList
        |> (fun scores -> scores.[(List.length data) / 2])
        
    incompleteData >> completionScore >> winner

let parse (rawInput: string) =
    rawInput.Split("\n")
    |> Seq.map (fun line -> line.ToCharArray() |> List.ofArray)
    |> List.ofSeq

let solve (input: string) =
    let data = parse input
    (part_1 data, part_2 data)
