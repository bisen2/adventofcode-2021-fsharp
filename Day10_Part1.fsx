module Logic =

  type ChunkType =
    | Paren
    | Square
    | Curly
    | Angle

  type VerificationStatus =
    | Complete
    | Incomplete of UnfinishedChunks: ChunkType list
    | Corrupted of IllegalCharacter: char

  let verify input =
    let rec verifyImpl remaining (openChunks: ChunkType list) =
      match openChunks, remaining with
      | _,          '('::xs -> verifyImpl xs (Paren :: openChunks)
      | _,          '['::xs -> verifyImpl xs (Square :: openChunks)
      | _,          '{'::xs -> verifyImpl xs (Curly :: openChunks)
      | _,          '<'::xs -> verifyImpl xs (Angle :: openChunks)
      | Paren::cs,  ')'::xs -> verifyImpl xs cs
      | Square::cs, ']'::xs -> verifyImpl xs cs
      | Curly::cs,  '}'::xs -> verifyImpl xs cs
      | Angle::cs,  '>'::xs -> verifyImpl xs cs
      | [],         []      -> Complete
      | cs,         []      -> Incomplete cs
      | _,          x::_    -> Corrupted x
    verifyImpl (input |> Seq.toList) []

  let score char =
    match char with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

  let solution input =
    input
    |> Seq.map verify
    |> Seq.sumBy (function | Corrupted char -> score char | _ -> 0)

module Runner =
  open Logic
  open System

  let parse (input: string) =
    input.Split Environment.NewLine
    |> Array.map (fun str -> str.Trim())
    |> Array.filter (fun str -> str.Length > 0)

  let testData =
    "[({(<(())[]>[[{[]{<()<>>
    [(()[<>])]({[<{<<[]>>(
    {([(<{}[<>[]}>{[]{[(<()>
    (((({<>}<{<{<>}{[]{[]{}
    [[<[([]))<([[{}[[()]]]
    [{[{({}]{}}([{[{{{}}([]
    {<[[]]>}<{[{[{[]{()[[[]
    [<(<(<(<{}))><([]([]()
    <{([([[(<>()){}]>(<<{{
    <{([{{}}[<[[[<>{}]]]>[]]"
    |> parse

  let testResult = solution testData

  let prodData =
    __SOURCE_DIRECTORY__ + @"/day10-data.txt"
    |> System.IO.File.ReadAllText
    |> parse

  let prodResult = solution prodData

  printfn $"Day 10 Part 1 Test Result: {testResult}"

  printfn $"Day 10 Part 1 Prod Result: {prodResult}"
