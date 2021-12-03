#r "nuget: FSharp.Data"

module Logic =

  type Change =
    | NoChange
    | Increase
    | Decrease

  let calcIncreases (depths: int list) =
    let rec calcIncreasesImpl prev depths increases =
      match depths with
      | [] -> increases
      | x :: xs when x > prev -> calcIncreasesImpl x xs (Increase :: increases)
      | x :: xs -> calcIncreasesImpl x xs (Decrease :: increases)
    calcIncreasesImpl depths.[0] depths.[1..] [NoChange]

  let countIncreases depths =
    depths
    |> List.sumBy (function | Increase -> 1 | _ -> 0)

  let solution = calcIncreases >> countIncreases

module Runner =
  open Logic
  open FSharp.Data

  let testData = [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263 ]

  let testResults = solution testData

  [<Literal>]
  let prodDataPath = __SOURCE_DIRECTORY__ + @"/day1-data.csv"

  type ProdCsvProvider = CsvProvider<prodDataPath>

  let prodData =
    let data = ProdCsvProvider.Load prodDataPath
    data.Rows
    |> Seq.toList
    |> Seq.map (fun x -> x.Depths)
    |> Seq.toList

  let prodResults = solution prodData

  printfn $"Day 1 Part 1 Test result: {testResults}"
  printfn $"Day 1 Part 1 Prod result: {prodResults}"
