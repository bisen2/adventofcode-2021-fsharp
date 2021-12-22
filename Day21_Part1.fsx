module Logic =

  (*
    This solution aims to follow the MVU architecture. Although the behavior of the deterministic
    die makes a more simple mathematical calculation of the final state possible, this architecture
    would allow for playing with a non-deterministic die.

    Model: The type `State` holds all information pertaining to the current state of the game. At any
        point in time, a copy of a `State` object can fully describe the current game.

    Update: The `update` function provides the logic for moving from one state to another. This encodes
        the logic of performing one turn of the game.

    View: The `view` function can produce a visual representation of the current game when given a copy
        of the `State` object. This allows for clean seperation of visual representation from game logic.
  *)

  [<Literal>]
  let WinningScore = 1000

  [<Literal>]
  let MaxPosition = 10

  [<Literal>]
  let MaxDieRoll = 100

  /// Type containing information about a specific player's state
  type Player =
    { Id: int
      Score: int
      Position: int }

  /// Type containing the full state of the game
  type State =
    { TurnIndex: int
      Players: Player list
      DieSeed: int }

  /// Given the index of a roll (seed), produces the result of rolling the deterministic die
  let rollDeterministicDie seed = seed % MaxDieRoll

  /// Given the current state of the game, performs one turn and returns the updated state
  let update (model: State) =
    let currentPlayer =
      model.Players
      |> List.filter (fun x -> x.Id = (model.TurnIndex - 1) % model.Players.Length + 1)
      |> List.exactlyOne
    let rolls =
      [ 0 .. 2 ]
      |> List.map (fun x -> rollDeterministicDie (model.DieSeed + x))
    let newPosition = (currentPlayer.Position + List.sum rolls) % MaxPosition |> function x when x = 0 -> 10 | x -> x
    let newScore = currentPlayer.Score + newPosition
    let updatedPlayer = { currentPlayer with Score = newScore; Position = newPosition; }
    let updatedPlayers = updatedPlayer :: (model.Players |> List.filter (fun x -> x <> currentPlayer))
    { model with
        Players = updatedPlayers
        TurnIndex = model.TurnIndex + 1
        DieSeed = model.DieSeed + 3 }

  /// Given a state, produces a visual representation of it
  let view (model: State) =
    let view (player: Player) = $"Player {player.Id}: Score {player.Score}, Position {player.Position}"
    let playerViews =
      model.Players
      |> List.map view
      |> String.concat "\n\t"
    $"Turn {model.TurnIndex}, Die Seed {model.DieSeed}:\n\t{playerViews}"

  /// Given an initial state, performs updates until the win condition is satisfied
  let rec run (model: State) =
    // printfn $"{view model}" // uncomment this line to enable viewing of game states
    let gameOver =
      model.Players
      |> List.exists (fun x -> x.Score >= WinningScore)
    if gameOver then
      model
    else
      run (update model)

  /// Given the final state of a game, calculates the product of the loser's score and the number of times the die was rolled
  let calcScore (model: State) =
    let loserScore =
      model.Players
      |> List.filter (fun x -> x.Score < WinningScore)
      |> List.sumBy (fun p -> p.Score)
    loserScore * (model.DieSeed - 1)

  /// Given an initial state, runs the update function until the win condition is met, then calculates the score of the final game state
  let solution init =
    run init
    |> calcScore

module Runner =
  open Logic

  let testPlayers =
    [ { Id = 1; Score = 0; Position = 4; }
      { Id = 2; Score = 0; Position = 8; } ]

  let testInit =
    { TurnIndex = 1
      Players = testPlayers
      DieSeed = 1 }

  let prodPlayers =
    [ { Id = 1; Score = 0; Position = 8 }
      { Id = 2; Score = 0; Position = 3 } ]

  let prodInit = { testInit with Players = prodPlayers }

  let testResult = solution testInit

  let prodResult = solution prodInit

  printfn $"Day 21 Part 1 Test Result: {testResult}"

  printfn $"Day 21 Part 1 Production Result: {prodResult}"
