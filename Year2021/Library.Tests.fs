module Year2021.LibraryTests

open Xunit

[<Fact>]
let ``Day 1`` () = Assert.Equal((1139, 1103), Solution.run 1)

