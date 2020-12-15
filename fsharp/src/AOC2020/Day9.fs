namespace AdventOfCode.Year2020
module Day9 =
  open AdventOfCode.Util

  let minMax xs = (List.min xs), (List.max xs)

  let sol1 preamble input =
    let isValid buf x =
      List.exists (fun n -> List.contains (x - n) buf) buf

    let rec loop buf (x::xs) = 
      if isValid buf x then
        loop ((List.tail buf) @ [x]) xs
      else
        x

    loop (List.take preamble input) (List.skip preamble input)

  let sol2 target inputs =
    let rec maybeGoal buf ns = 
      match List.sum buf with
      | total when total = target -> Some buf
      | total when total > target -> None
      | _ ->
        match ns with
        | [] -> None
        | n::ns -> maybeGoal (n::buf) ns

    let rec loop xs =
      match maybeGoal [] xs with
      | None -> loop (List.tail xs)
      | Some ns ->
        let (m, n) = minMax ns
        m + n

    loop inputs

  let sample = [
    bigint 35;
    bigint 20;
    bigint 15;
    bigint 25;
    bigint 47;
    bigint 40;
    bigint 62;
    bigint 55;
    bigint 65;
    bigint 95;
    bigint 102;
    bigint 117;
    bigint 150;
    bigint 182;
    bigint 127;
    bigint 219;
    bigint 299;
    bigint 277;
    bigint 309;
    bigint 576;
  ]
  let input = List.ofSeq <| readBigInts (inputData 2020 9)

  let main () =
    printfn "%A" (sol1 25 input)
    printfn "%A" (sol2 (sol1 25 input) input)
