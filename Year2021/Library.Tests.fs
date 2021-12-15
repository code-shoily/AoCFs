module Year2021.LibraryTests

open Xunit

[<Fact>]
let ``Day 1`` () = Assert.Equal("(1139, 1103)", Solution.run 1)

[<Fact>]
let ``Day 10`` () = Assert.Equal("(339411, 2289754624L)", Solution.run 10)

[<Fact>]
let ``Day 14`` () = Assert.Equal("(3118UL, 4332887448171UL)", Solution.run 14)

