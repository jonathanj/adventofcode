namespace AdventOfCode.Year2020
module Day15 =
  open AdventOfCode.Util

  type State =
    { Last: bool * int;
      Seen: Map<int, int * int>; }

  let parse (input: string) =
    Seq.map (fun n -> System.Int32.Parse(n)) (input.Split(","))

  let sol1 input limit =
    let seen state n turn =
      Map.change
        n
        (fun v ->
          match v with
          | Some (a, _) -> Some (turn, a)
          | None -> Some (turn, turn))
        state.Seen

    let rec loop state turn =
      let (isNew, last) = state.Last
      if turn = (limit + 1) then
        last
      else
        let spoken =
          if isNew then
            0
          else
            let (a, b) = state.Seen.[last]
            a - b
        loop { state with Last = (not <| Map.containsKey spoken state.Seen, spoken); Seen = seen state spoken turn}  (turn + 1)

    let state =
      { Last = (true, Seq.last input);
        Seen = Seq.mapi (fun i n -> n, (i + 1, i + 1)) input |> Map.ofSeq; }
    loop state ((Seq.length input) + 1)

  let sol2 input =
    42

  let sample = parse "0,3,6"
  let sample2 = parse "3,1,2"

  let input = readLines (inputData 2020 15) |> Seq.head |> parse

  let main () =
    printfn "%A" (sol1 input 2020)
    // TODO: I assume the correct way to do this is to implement cycle detection
    // then figure out when the cycle will repeat for the 30 millionth time.
    printfn "%A" (sol1 input 30_000_000)
