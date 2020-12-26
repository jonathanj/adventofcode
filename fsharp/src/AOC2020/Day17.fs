namespace AdventOfCode.Year2020
module Day17 =
  open AdventOfCode.Util

  type State =
    { World: Map<int * int * int * int, bool>
    ; Size: int * int
    ; Gen: int }

  let parse input =
    let world = Map.ofSeq (seq {
      for y, line in Seq.indexed input do
        for x, ch in Seq.indexed line do
          ((x, y, 0, 0), ch = '#')
    })
    let size = (Seq.length (Seq.head input), Seq.length input)
    { World = world; Gen = 1; Size = size }

  let neighbours (state: State) (x, y, z, _) =
    seq { 
      for ox in [-1..1] do
        for oy in [-1..1] do
          for oz in [-1..1] do
            let coord = (x + ox, y + oy, z + oz, 0)
            if (ox, oy, oz) <> (0, 0, 0) then
              let isActive = Option.defaultValue false (Map.tryFind coord state.World)
              (coord, isActive)
    }

  let neighbours4d (state: State) (x, y, z, w) =
    seq { 
      for ow in [-1..1] do
        for ox in [-1..1] do
          for oy in [-1..1] do
            for oz in [-1..1] do
              let coord = (x + ox, y + oy, z + oz, w + ow)
              if (ox, oy, oz, ow) <> (0, 0, 0, 0) then
                let isActive = Option.defaultValue false (Map.tryFind coord state.World)
                (coord, isActive)
    }

  let render (state: State) =
    let (w, h) = state.Size
    let n = state.Gen - 1
    for z in [-n..n] do
      printfn "z=%d" z
      for y in [(0 - n)..(h - 1 + n)] do
        for x in [(0 - n)..(w - 1 + n)] do
          match Map.tryFind (x, y, z, 0) state.World with
          | Some true -> printf "#"
          | _ -> printf "."
        printfn ""

  let worldCoords (state: State) =
    let (w, h) = state.Size
    let n = state.Gen
    seq {
      for z in [-n..n] do
        for y in [(0 - n)..(h - 1 + n)] do
          for x in [(0 - n)..(w - 1 + n)] do
            yield (x, y, z, 0)
    }

  let worldCoords4d (state: State) =
    let (w, h) = state.Size
    let n = state.Gen
    seq {
      for ww in [-n..n] do
        for z in [-n..n] do
          for y in [(0 - n)..(h - 1 + n)] do
            for x in [(0 - n)..(w - 1 + n)] do
              yield (x, y, z, ww)
    }

  let generate worldCoords neighbours (state: State) =
    let foo m pos =
      let isActive = Map.tryFind pos state.World |> Option.defaultValue false
      let numActiveNeighbours = Seq.filter snd (neighbours state pos) |> Seq.length
      let newIsActive =
        match (isActive, numActiveNeighbours) with
        | (true, 2) | (true, 3) -> true
        | (false, 3) -> true
        | _ -> false
      Map.add pos newIsActive m
    {state with World = Seq.fold foo state.World (worldCoords state); Gen = state.Gen + 1 }

  let sol1 state =
    let final = Seq.take 7 (iterate (generate worldCoords neighbours) state) |> Seq.last
    final.World
    |> vals
    |> Seq.filter id
    |> Seq.length

  let sol2 state =
    let final = Seq.take 7 (iterate (generate worldCoords4d neighbours4d) state) |> Seq.last
    final.World
    |> vals
    |> Seq.filter id
    |> Seq.length

  let sample = parse [
    ".#.";
    "..#";
    "###";
  ]

  let input = readLines (inputData 2020 17) |> parse

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
