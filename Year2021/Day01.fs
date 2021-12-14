module Year2021.Day1

let parse (data: string) = data.Split "\n" |> Seq.map int

let part_1 (data: int seq) =
    data
    |> Seq.pairwise
    |> Seq.filter (fun (a, b) -> b > a)
    |> Seq.length

let part_2 (data: int seq) =
    data
    |> Seq.windowed 4
    |> Seq.filter (fun window -> window[3] > window[0])
    |> Seq.length

let solve (input: string) =
    let parsed = parse input
    (part_1 parsed, part_2 parsed)