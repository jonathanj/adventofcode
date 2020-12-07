namespace AdventOfCode.Year2020
module Day1 =
  open AdventOfCode.Util

  let sample = [
    1721;
    979;
    366;
    299;
    675;
    1456;
  ]

  let part1 (input: list<int>) : int =
    seq {
      for a in input do
      for b in input do
      if a + b = 2020 then a * b
    } |> Seq.head

  let part2 (input: list<int>) : int =
    seq {
      for a in input do
      for b in input do
      for c in input do
      if a + b + c = 2020 then a * b * c
    } |> Seq.head

  let input = readInts (inputData 2020 1) |> List.ofSeq

  let main () =
    printfn "%A" (part1 input)
    printfn "%A" (part2 input)
