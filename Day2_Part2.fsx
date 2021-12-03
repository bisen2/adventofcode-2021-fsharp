#load "./Day2_Part1.fsx"

module Logic =
  open Day2_Part1.Logic

  /// Type containing positional information
  type Position =
    { Horizontal: int
      Depth: int
      Aim: int }
    static member Zero =
      { Horizontal = 0
        Depth = 0
        Aim = 0 }

  /// Given a current position and instruction, generates the new position when that instruction is applied
  let updatePosition pos instr =
    match instr with
    | Down x -> { pos with Aim = pos.Aim + x }
    | Up x -> { pos with Aim = pos.Aim - x }
    | Forward x -> { pos with Horizontal = pos.Horizontal + x; Depth = pos.Depth + (x * pos.Aim) }

  let solution instr =
    instr
    |> Seq.map parseInstruction
    |> Seq.fold updatePosition Position.Zero
    |> fun pos -> pos.Horizontal * pos.Depth

module Runner =
  open Logic
  open Day2_Part1.Runner

  let testResults = solution testData

  let prodResults = solution prodData

  printfn $"Day 2 Part 2 Test result: {testResults}"
  printfn $"Day 2 Part 2 Prod result: {prodResults}"
