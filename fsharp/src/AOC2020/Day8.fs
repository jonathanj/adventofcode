namespace AdventOfCode.Year2020
module Day8 =
  open AdventOfCode.Util

  type Op = string * int

  let parse inputs =
    let parseInst [| op; arg |] = (op, int arg)
    List.ofSeq <| Seq.mapi
      (fun idx (inst: string) ->
        (idx, (parseInst (inst.Split " ")))) inputs

  let solve insts =
    let rec loop ip acc seen =
      if (ip >= (List.length insts)) then
        (true, acc)
      else
        let (n, inst) = insts.[ip]
        if (Set.contains n seen) then
          (false, acc)
        else
          match inst with
          | ("acc", n) -> loop (ip + 1) (acc + n) (Set.add ip seen)
          | ("nop", _) -> loop (ip + 1) acc (Set.add ip seen)
          | ("jmp", n) -> loop (ip + n) acc (Set.add ip seen)
          | _ -> failwith "Unknown op"
    loop 0 0 Set.empty

  let sol1 insts =
    snd (solve insts)

  let sol2 inputs =
    let modify before =
      let (idx, (op, n)) =
        List.findBack
          (fun (idx, (op, n)) ->
            if idx > before then
              false
            else
              match op with
              | "nop" | "jmp" -> true
              | _ -> false) inputs
      let flippedOp =
        match op with
        | "jmp" -> "nop"
        | "nop" -> "jmp"
        | _ -> failwith "Unexpected op"
      let (l, r) = List.splitAt idx inputs
      l @ [(idx, (flippedOp, n))] @ (List.tail r)

    let rec loop before =
      match solve (modify before) with
      | (true, n) -> n
      | (false, _) -> loop (before - 1)

    loop (List.length inputs)

  let sample = parse <| Seq.ofList [
    "nop +0";
    "acc +1";
    "jmp +4";
    "acc +3";
    "jmp -3";
    "acc -99";
    "acc +1";
    "jmp -4";
    "acc +6";
  ]
  let input = parse <| readLines (inputData 2020 8)

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
