module AdventOfCode.D02.SafetyTests

open Xunit
open Resolution

[<Theory>]
[<InlineData("7 6 4 2 1")>]
[<InlineData("1 3 6 7 9")>]
let ``report is safe`` report =
    let safe = isSafe report
    Assert.True safe

[<Theory>]
[<InlineData("1 2 7 8 9")>]
[<InlineData("9 7 6 2 1")>]
[<InlineData("1 3 2 4 5")>]
[<InlineData("8 6 4 4 1")>]
let ``report is unsafe`` report =
    let safe = isSafe report
    Assert.False safe