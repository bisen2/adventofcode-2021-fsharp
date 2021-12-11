module Logic =

  /// Given a point, get a list of all neighboring points (handling edge cases)
  let getNeighbors map (x, y) =
    let maxX = Array2D.length1 map - 1
    let maxY = Array2D.length2 map - 1
    [ (x-1, y); (x+1, y); (x, y-1); (x, y+1) ]
    |> List.filter (fun (x, y) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY)

  /// Given a point, check that it is a local minimum
  let isLowerThanNeighbors map (x, y) =
    let lowestNeighbor =
      getNeighbors map (x, y)
      |> List.map (fun (x, y) -> map[x, y])
      |> List.min
    map[x,y] < lowestNeighbor

  /// Given a map, get a list of all local minima
  let findLocalMinima map =
    (Array2D.length1 map - 1, Array2D.length2 map - 1)
    |> fun (maxX, maxY) -> [ 0 .. maxX ], [ 0 .. maxY ]
    |> fun (xs, ys) -> List.allPairs xs ys
    |> List.filter (fun pos -> isLowerThanNeighbors map pos)

  /// Given a map, get the sum of the heights of all local minima
  let solution map =
    findLocalMinima map
    |> List.map (fun (x,y) -> map[x,y] + 1)
    |> List.sum

module Runner =
  open Logic
  open System

  let parseInput (input: string) =
    input.Split Environment.NewLine
    |> Array.map (fun line -> line.Trim())
    |> Array.filter (fun line -> line.Length > 0)
    |> Array.map Seq.toArray
    |> Array.map (Array.map (string >> Int32.Parse))
    |> fun nestedArrays -> Array2D.init (nestedArrays |> Seq.length) (nestedArrays |> Seq.head |> Seq.length) (fun x y -> nestedArrays[x][y])

  let testData =
    "2199943210
    3987894921
    9856789892
    8767896789
    9899965678"
    |> parseInput

  let testResult = solution testData

  let prodData =
    __SOURCE_DIRECTORY__ + @"/day9-data.txt"
    |> System.IO.File.ReadAllText
    |> parseInput

  let prodResult = solution prodData

  printfn $"Day 9 Part 1 Test Result: {testResult}"

  printfn $"Day 9 Part 1 Prod Result: {prodResult}"
