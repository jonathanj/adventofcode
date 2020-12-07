namespace AdventOfCode.Year2020
module Day5 =
  open AdventOfCode.Util

  let seatID (s: string) =
    Seq.map
      (fun ch ->
        match ch with
          | 'B' | 'R' -> '1'
          | 'F' | 'L' -> '0'
          | _ -> failwith "Invalid character") s
    |> System.String.Concat |> binaryToInt32

  let sol1 inputs =
    Seq.max (Seq.map seatID inputs)

  let sol2 inputs =
    let allSeats = Set.ofList [8 .. (sol1 inputs)]
    Set.ofSeq (Seq.map seatID inputs)
    |> Set.difference allSeats
    |> List.ofSeq
    |> List.exactlyOne

  let sample = [
    "FBFBBFFRLR";
    "BFFFBBFRRR";
    "FFFBBBFRRR";
    "BBFFBBFRLL" ]
  let input = readLines (inputData 2020 5)

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)