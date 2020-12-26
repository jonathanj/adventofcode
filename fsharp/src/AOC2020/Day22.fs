namespace AdventOfCode.Year2020
module Day22 =
  open AdventOfCode.Util

  type Player = PlayerOne | PlayerTwo

  let parse input =
    match List.ofSeq input with
    | [ p1; p2 ] ->
        let parseHand s = Seq.map int (lines s |> Seq.tail) |> List.ofSeq
        (parseHand p1, parseHand p2)
    | _ -> failwith "Malformed input"

  let scoreHand hand =
    List.fold
      (fun result (i, n) -> result + (i * n))
      0
      (List.zip [List.length hand .. -1 .. 1] hand)

  let sol1 (p1, p2) =
    let rec round p1 p2 =
      match (p1, p2) with
      | (_, []) -> p1
      | ([], _) -> p2
      | (x::xs, y::ys) ->
          if x > y then
            round (xs @ [x; y]) ys
          else
            round xs (ys @ [y; x])

    round p1 p2 |> scoreHand

  let sol2 (p1, p2) =
    let rec round seen p1 p2 =
      if Set.contains p1 seen || Set.contains p2 seen then
        (PlayerOne, p1)
      else
        match (p1, p2) with
        | (_, []) -> (PlayerOne, p1)
        | ([], _) -> (PlayerTwo, p2)
        | (x::xs, y::ys) ->
            let newSeen = Set.union (Set [p1; p2]) seen
            if List.length xs < x || List.length ys < y then
              if x > y then
                round newSeen (xs @ [x; y]) ys
              else
                round newSeen xs (ys @ [y; x])
            else
              match round Set.empty (List.take x xs) (List.take y ys) with
              | (PlayerOne, _) -> round newSeen (xs @ [x; y]) ys
              | (PlayerTwo, _) -> round newSeen xs (ys @ [y; x])
      
    round Set.empty p1 p2 |> snd |> scoreHand

  let sample = parse <| [
    "Player 1:\n9\n2\n6\n3\n1";
    "Player 2:\n5\n8\n4\n7\n10";
  ]

  let input = readNewlineGroups (inputData 2020 22) |> parse

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
