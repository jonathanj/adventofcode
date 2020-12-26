namespace AdventOfCode.Year2020
module Day24 =
  open AdventOfCode.Util

  type Direction =
    | West
    | East
    | NorthWest
    | NorthEast
    | SouthWest
    | SouthEast

  let parse input =
    let rec parseTileInstructions chars =
      match chars with
      | [] -> []
      | 'e'::chars -> East :: parseTileInstructions chars
      | 'w'::chars -> West :: parseTileInstructions chars
      | 's'::chars ->
          match chars with
          | 'e'::chars -> SouthEast :: parseTileInstructions chars
          | 'w'::chars -> SouthWest :: parseTileInstructions chars
          | _ -> failwith "Malformed direction"
      | 'n'::chars ->
          match chars with
          | 'e'::chars -> NorthEast :: parseTileInstructions chars
          | 'w'::chars -> NorthWest :: parseTileInstructions chars
          | _ -> failwith "Malformed direction"
      | _ -> failwith "Malformed input"

    Seq.map (List.ofSeq >> parseTileInstructions) input

  let step (x, y, z) dir =
    match dir with
    | West      -> (x - 1, y + 1, z)
    | East      -> (x + 1, y - 1, z)
    | NorthWest -> (x,     y + 1, z - 1)
    | NorthEast -> (x + 1, y,     z - 1)
    | SouthWest -> (x - 1, y,     z + 1)
    | SouthEast -> (x,     y - 1, z + 1)

  let inline neighbours pos =
    Seq.map (step pos) [West; East; NorthWest; NorthEast; SouthWest; SouthEast]

  let tilePositions instructions =
    Seq.map (Seq.fold step (0, 0, 0)) instructions |> List.ofSeq

  let sol1 instructions =
    tilePositions instructions
    |> frequencies
    |> Map.toList
    |> List.filter (fun (pos, count) -> count % 2 <> 0)
    |> List.length

  let dictToMap (dic : System.Collections.Generic.IDictionary<_,_>) = 
    dic 
    |> Seq.map (|KeyValue|)  
    |> Map.ofSeq

  let mapToDict map =
    map 
    |> Map.toSeq
    |> dict

  let sol2 instructions =
    let inline touchNeighbours (positions: System.Collections.Generic.Dictionary<int*int*int,int>) =
      let mutable newM = System.Collections.Generic.Dictionary(positions)
      for outerPos in positions.Keys do
        for pos in (neighbours outerPos) do
          if not (positions.ContainsKey pos) then
            newM.[pos] <- 0
      newM

    let inline loop (positions: System.Collections.Generic.Dictionary<int*int*int,int>) =
      let mutable result = System.Collections.Generic.Dictionary(positions)
      for kvp in positions do
        let pos = kvp.Key
        let ns = neighbours pos
        let blackNeighbours = Seq.sumBy (fun p -> if positions.ContainsKey(p) then positions.[p] else 0) ns
        match (kvp.Value, blackNeighbours) with
        | (1, x) when x = 0 || x > 2 -> result.[pos] <- 0
        | (0, 2) -> result.[pos] <- 1
        | _ -> ()
      result

    let mutable state =
      tilePositions instructions
      |> frequencies
      |> Map.map (fun _ count -> if count % 2 <> 0 then 1 else 0)
      |> mapToDict
      |> System.Collections.Generic.Dictionary
    for i = 1 to 100 do
      state <- loop (touchNeighbours state)
    Seq.sum state.Values

  let sample = parse (Seq.ofList [
    "sesenwnenenewseeswwswswwnenewsewsw";
    "neeenesenwnwwswnenewnwwsewnenwseswesw";
    "seswneswswsenwwnwse";
    "nwnwneseeswswnenewneswwnewseswneseene";
    "swweswneswnenwsewnwneneseenw";
    "eesenwseswswnenwswnwnwsewwnwsene";
    "sewnenenenesenwsewnenwwwse";
    "wenwwweseeeweswwwnwwe";
    "wsweesenenewnwwnwsenewsenwwsesesenwne";
    "neeswseenwwswnwswswnw";
    "nenwswwsewswnenenewsenwsenwnesesenew";
    "enewnwewneswsewnwswenweswnenwsenwsw";
    "sweneswneswneneenwnewenewwneswswnese";
    "swwesenesewenwneswnwwneseswwne";
    "enesenwswwswneneswsenwnewswseenwsese";
    "wnwnesenesenenwwnenwsewesewsesesew";
    "nenewswnwewswnenesenwnesewesw";
    "eneswnwswnwsenenwnwnwwseeswneewsenese";
    "neswnwewnwnwseenwseesewsenwsweewe";
    "wseweeenwnesenwwwswnew";
  ])

  let input = readLines (inputData 2020 24) |> parse

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
