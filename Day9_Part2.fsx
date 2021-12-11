#load "./Day9_Part1.fsx"

module Logic =
  open Day9_Part1.Logic

  /// Given a local minimum, find the points within its basin
  /// Note: Does not check that the given point is actually a local minimum
  let getBasin map (x, y) =
    let rec getBasinImpl map (x, y) acc =
      let neighborsInBasin =
        getNeighbors map (x, y)
        |> List.filter (fun (thisX, thisY) -> map[thisX, thisY] > map[x, y] && map[thisX, thisY] < 9)
      match neighborsInBasin with
      | [] -> acc
      | xs ->
          xs
          |> List.map (fun pos -> getBasinImpl map pos (xs @ acc))
          |> List.concat
    (x, y) :: getBasinImpl map (x, y) []
    |> List.distinct

  /// Given a map, find the product of the sizes of the three largest basins
  let solution input =
    findLocalMinima input
    |> List.map (getBasin input >> List.length)
    |> List.sortDescending
    |> List.take 3
    |> List.fold (*) 1

module Runner =
  open Logic
  open Day9_Part1.Runner

  let testResult = solution testData

  let prodResult = solution prodData

  printfn $"Day 9 Part 2 Test Result: {testResult}"

  printfn $"Day 9 Part 2 Prod Result: {prodResult}"
