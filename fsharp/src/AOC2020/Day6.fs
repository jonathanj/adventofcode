namespace AdventOfCode.Year2020
module Day6 =
  open AdventOfCode.Util

  let sol1 (inputs: seq<seq<string>>) =
    Seq.sumBy ((String.concat "") >> Set.ofSeq >> Set.count) inputs

  let sol2 (inputs: seq<seq<string>>) =
    Seq.sumBy ((Seq.map Set.ofSeq) >> Set.intersectMany >> Set.count) inputs

  let sample = Seq.map lines (Seq.ofList [
    "abc";
    "a\nb\nc";
    "ab\nac";
    "a\na\na\na";
    "b";
  ])
  let input = Seq.map lines (readNewlineGroups (inputData 2020 6))

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
