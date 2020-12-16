namespace AdventOfCode.Year2020
module Day9 =
  open AdventOfCode.Util

  let minMax xs = (List.min xs), (List.max xs)

  let sol1 preamble input =
    let isValid buf x =
      List.exists (fun n -> List.contains (x - n) buf) buf

    let rec loop buf xs =
      match xs with
      | x::xs when isValid buf x -> loop ((List.tail buf) @ [x]) xs
      | x::_ -> x
      | _ -> failwith "Nope"

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
    35L;
    20L;
    15L;
    25L;
    47L;
    40L;
    62L;
    55L;
    65L;
    95L;
    102L;
    117L;
    150L;
    182L;
    127L;
    219L;
    299L;
    277L;
    309L;
    576L;
  ]
  let input = List.ofSeq <| readInt64s (inputData 2020 9)

  let main () =
    printfn "%A" (sol1 25 input)
    printfn "%A" (sol2 (sol1 25 input) input)
