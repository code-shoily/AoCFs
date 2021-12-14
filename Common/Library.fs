namespace Common

open System.IO

module InputReader =
    let getInputFileName directory day =
        let day = day |> sprintf "%02i"
        Path.Combine(directory, $"inputs/day_{day}.txt")
    
    let readInputFile directory day =
        day |> getInputFileName directory |> File.ReadAllText

module Formatter =
    let output data = $"%A{data}"
        