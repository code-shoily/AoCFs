// Advent of Code 2021
// --- Day 14: Extended Polymerization ---
// Link: https://adventofcode.com/2021/day/14
module Year2021.Day14

type Polymer =
    { Rules: Map<char * char, char>
      Counter: Map<char, uint64>
      Pairs: Map<char * char, uint64> }

let parse (data: string) =
    let parseRules (rules: string) =
        rules.Split("\n")
        |> Seq.map
            (fun rule ->
                match rule.Split(" -> ") with
                | [| adjacent; insertion |] ->
                    let pairs = adjacent.ToCharArray()
                    ((pairs[0], pairs[1]), insertion.ToCharArray()[0])
                | _ -> failwith "Invalid rule")
        |> Map.ofSeq

    let (template, rules) =
        match List.ofSeq <| data.Split("\n\n") with
        | template :: rules -> (template, parseRules <| List.head rules)
        | _ -> failwith "Invalid polymer"

    { Rules = rules
      Counter =
          template
          |> Seq.countBy id
          |> Seq.map (fun (k, v) -> (k, uint64 v))
          |> Map.ofSeq
      Pairs =
          template
          |> Seq.pairwise
          |> Seq.groupBy id
          |> Seq.map (fun (a, b) -> (a, b |> Seq.length |> uint64))
          |> Map.ofSeq }

let react (polymer: Polymer) =
    let updateCounter newCount op =
        function
        | Some value -> Some(op value newCount)
        | None -> Some(newCount)

    polymer.Pairs
    |> Seq.fold
        (fun acc kv ->
            let a, b = kv.Key
            let count = kv.Value
            let insert = polymer.Rules[(a, b)]

            let newPairs =
                acc.Pairs
                |> Map.change (a, b) (updateCounter count (-))
                |> Map.change (a, insert) (updateCounter count (+))
                |> Map.change (insert, b) (updateCounter count (+))

            let newCounter =
                acc.Counter
                |> Map.change insert (updateCounter count (+))

            { Rules = acc.Rules
              Counter = newCounter
              Pairs = newPairs })
        polymer

let reaction (polymer: Polymer) (repeat: int) =
    seq { 1 .. repeat }
    |> Seq.fold (fun acc _ -> react acc) polymer
    |> (fun { Counter = counter } -> (Seq.max counter.Values, Seq.min counter.Values))
    ||> (-)

let part_1 (polymer: Polymer) = reaction polymer 10
let part_2 polymer = reaction polymer 40

let solve (data: string) =
    let data = parse data
    (part_1 data, part_2 data)
