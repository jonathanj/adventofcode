namespace AdventOfCode.Year2020
module Day25 =
  open AdventOfCode.Util

  let parse input =
    match List.ofSeq input with
    | [a; b] -> (a, b)
    | _ -> failwith "Malformed input"

  let transformNumbers subj =
    seq {
      let mutable i = 0
      let mutable n = 1
      while true do
        i <- i + 1
        n <- (n * subj) % 20201227
        yield (i, n)
    }

  let sol1 (a, b) =
    let aLoopSize =
      Seq.skipWhile (fun (_, n) -> n <> a) (transformNumbers 7)
      |> Seq.head
      |> fst
    Seq.skipWhile (fun (i, _) -> i <> aLoopSize) (transformNumbers b)
    |> Seq.head
    |> snd

  let sample = parse ([ 5764801; 17807724 ])

  let input = parse <| readInts (inputData 2020 25)

  let main () =
    assert (sol1 sample = 14897079)
    printfn "%A" (sol1 input)
