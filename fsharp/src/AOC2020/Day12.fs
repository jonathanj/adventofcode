namespace AdventOfCode.Year2020
module Day12 =
  open AdventOfCode.Util

  type Position = int * int
  type ShipState =
    { Direction: char;
      Position: Position }
  type WaypointState =
    { Position: Position;
      ShipPosition: Position }
  type Instruction = char * int

  let parseInstruction (inst: string) =
    match tryParseInt (inst.Substring(1)) with
    | Some n -> (inst.[0], n)
    | _ -> failwith "Unknown instruction"

  let manhattanDistance ((x, y): Position) = 
    (abs x) + (abs y)

  let sol1 input =
    let rec move (state: ShipState) (inst: Instruction) =
      let turn (state: ShipState) ((ch, n): Instruction) =
        let turns = ['N'; 'E'; 'S'; 'W']
        let idx =
          (List.findIndex ((=) state.Direction) turns) +
          (if ch = 'L' then 360 - n else 360 + n) / 90
        { state with Direction = turns.[idx % (List.length turns)] }

      let (x, y) = state.Position
      match inst with
      | ('N', n) -> { state with Position = (x, y - n) }
      | ('S', n) -> { state with Position = (x, y + n) }
      | ('E', n) -> { state with Position = (x + n, y) }
      | ('W', n) -> { state with Position = (x - n, y) }
      | ('F', n) -> move state (state.Direction, n)
      | (('L' | 'R'), _) -> turn state inst
      | _ -> failwithf "Unknown instruction %A" inst

    List.fold move {Direction = 'E'; Position = (0, 0)} input
    |> (fun state -> manhattanDistance state.Position)

  let sol2 input =
    // NOTE: These rotations look weird because we opted for north to be
    // negative, so the Y-axis is inverted.
    let rotateLeft ((x, y): Position) degrees =
      match degrees with
      | 90 -> (y, -x)
      | 180 -> (-x, -y)
      | 270 -> (-y, x)
      | _ -> (x, y)

    let rotateRight ((x, y): Position) degrees =
      match degrees with
      | 90 -> (-y, x)
      | 180 -> (-x, -y)
      | 270 -> (y, -x)
      | _ -> (x, y)

    let execute (state: WaypointState) (inst: Instruction) =
      let (x, y) = state.Position
      match inst with
      | ('N', n) -> { state with Position = (x, y - n) }
      | ('S', n) -> { state with Position = (x, y + n) }
      | ('E', n) -> { state with Position = (x + n, y) }
      | ('W', n) -> { state with Position = (x - n, y) }
      | ('L', n) -> { state with Position = rotateLeft state.Position n }
      | ('R', n) -> { state with Position = rotateRight state.Position n }
      | ('F', n) ->
        let (sx, sy) = state.ShipPosition
        { state with ShipPosition = (sx + (x * n), sy + (y * n)) }
      | _ -> failwith "Invalid instruction"

    List.fold execute { ShipPosition = (0, 0); Position = (10, -1) } input
    |> (fun state -> manhattanDistance state.ShipPosition)

  let sample = List.map parseInstruction [
    "F10";
    "N3";
    "F7";
    "R90";
    "F11";
  ]

  let input =
    readLines (inputData 2020 12)
    |> Seq.map parseInstruction
    |> List.ofSeq

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
