#load "./Day1_Part1.fsx"

module Logic =
  open Day1_Part1.Logic

  /// Given a list of integers, generates the three-value rolling sum of them
  let threeMeasSums values =
    let rec threeMeasSumImpl values sums =
      match values with
      | x :: y :: z :: xs -> threeMeasSumImpl (y :: z :: xs) (x + y + z :: sums)
      | _ -> sums
    threeMeasSumImpl values [] |> List.rev

  let solution = threeMeasSums >> calcIncreases >> countIncreases

module Runner =
  open Logic
  open Day1_Part1.Runner

  let testResults = solution testData

  let prodResults = solution prodData

  printfn $"Day 1 Part 2 Test result: {testResults}"
  printfn $"Day 1 Part 2 Prod result: {prodResults}"
