namespace Year2021

open Common.Formatter
open Common.InputReader

module Solution =
    let get_input = readInputFile __SOURCE_DIRECTORY__
    let run day =
        let inputData = readInputFile __SOURCE_DIRECTORY__ day
        match day with
        | 1 -> output <| Day1.solve inputData
        | 10 -> output <| Day10.solve inputData
        | 14 -> output <| Day14.solve inputData
        | _ -> failwith $"Year 2021 Day #{day} is not solved yet."