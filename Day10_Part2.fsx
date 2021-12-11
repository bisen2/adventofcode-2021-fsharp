#load "./Day10_Part1.fsx"

module Logic =
  open Day10_Part1.Logic

  let closure chunk =
    match chunk with
    | Paren -> ')'
    | Square -> ']'
    | Curly -> '}'
    | Angle -> '>'

  let buildClosure = List.map closure

  let scoreChar char =
    match char with
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L
    | _   -> 0L

  let scoreClosure = Seq.fold (fun acc x -> acc * 5L + scoreChar x) 0L

  let winner scores =
    scores
    |> Seq.toList
    |> List.sort
    |> fun scores -> scores.[ List.length scores / 2 ]

  let solution input =
    input
    |> Seq.map verify
    |> Seq.choose (function | Incomplete x -> Some x | _ -> None)
    |> Seq.map buildClosure
    |> Seq.map scoreClosure
    |> winner

module Runner =
  open Logic
  open Day10_Part1.Runner

  let testResult = solution testData

  let prodResult = solution prodData

  printfn $"Day 10 Part 2 Test Result: {testResult}"

  printfn $"Day 10 Part 2 Prod Result: {prodResult}"
