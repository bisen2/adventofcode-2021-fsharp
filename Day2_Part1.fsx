#r "nuget: FSharp.Data"

module Logic =

  type Instruction =
    | Forward of int
    | Down of int
    | Up of int

  type Position =
    { Horizontal: int
      Depth: int }
    static member Zero =
      { Horizontal = 0
        Depth = 0 }

  let updatePosition pos instr =
    match instr with
    | Forward x -> { pos with Horizontal = pos.Horizontal + x }
    | Down x -> { pos with Depth = pos.Depth + x }
    | Up x -> { pos with Depth = pos.Depth - x }

  let parseInstruction instr =
    match instr with
    | "forward", x -> Forward x
    | "down", x -> Down x
    | "up", x -> Up x
    | _ -> failwith $"Invalid instruction: {instr}"

  let solution instr =
    instr
    |> Seq.map parseInstruction
    |> Seq.fold updatePosition Position.Zero
    |> fun pos -> pos.Horizontal * pos.Depth

module Runner =
  open Logic
  open FSharp.Data

  let testData =
    [ ("forward", 5)
      ("down", 5)
      ("forward", 8)
      ("up", 3)
      ("down", 8)
      ("forward", 2) ]

  let testResult = solution testData

  [<Literal>]
  let prodDataPath = __SOURCE_DIRECTORY__ + @"/day2-data.csv"

  type ProdDataProvider = CsvProvider<prodDataPath>

  let prodData =
    let data = ProdDataProvider.Load prodDataPath
    data.Rows
    |> Seq.map (fun x -> x.Direction, x.Magnitude)

  let prodResult = solution prodData

  printfn $"Day 2 Part 1 Test result: {testResult}"
  printfn $"Day 2 Part 1 Prod result: {prodResult}"
