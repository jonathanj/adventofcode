namespace AdventOfCode.Year2020
module Day16 =
  open AdventOfCode.Util

  type Range = int64 * int64

  type Rule =
    { Name: string
    ; Ranges: Range * Range
    }

  type Program =
    { Rules: list<Rule>
    ; Mine: list<int64>
    ; Nearby: list<list<int64>>
    }

  let collateSet kf vf xs =
    let collate m x = Map.change (kf x) (fun values -> Some (List.append (Option.defaultValue List.empty values) [(vf x)])) m
    List.fold collate Map.empty xs
    |> Map.map (fun _ v -> Set.ofList v)
    |> Map.toList

  let parse (input: seq<string>) =
    let parseRule ruleString = 
      match ruleString with
      | ParseRegex "^(.+): (\d+)-(\d+) or (\d+)-(\d+)$" [String name; Integer64 am; Integer64 an; Integer64 bm; Integer64 bn] ->
        { Name = name; Ranges = ((am, an), (bm, bn))}
      | _ -> failwith "Invalid rule"

    let parseTickets (ticketsString: string) =
      Seq.map int64 (ticketsString.Split(","))
      |> List.ofSeq
    
    match List.ofSeq input with
    | [ rules; mine; nearby ] ->
      { Rules = List.map parseRule (lines rules |> List.ofSeq)
      ; Mine = parseTickets (lines mine |> Seq.tail |> Seq.head)
      ; Nearby = List.map parseTickets (lines nearby |> Seq.tail |> List.ofSeq)
      }
    | _ -> failwith "Invalid input"

  let invalidTicketValuesForRule ticket rule =
    let valueInRanges (((aa, ab), (ba, bb)): Range * Range) value =
      (value >= aa && value <= ab) || (value >= ba && value <= bb)
    Set.filter (not << (valueInRanges rule.Ranges)) (Set.ofList ticket)

  let invalidTicketValues rules ticket =
    List.map (invalidTicketValuesForRule ticket) rules
    |> Set.intersectMany

  let sol1 program =
    Seq.collect (invalidTicketValues program.Rules) program.Nearby
    |> Seq.reduce (+)

  let sol2 program =
    let withSingleMatch (_, idxs) = (Set.count idxs) = 1
    // Sets with only a single value represent a certainty, removing the
    // certainty from all other sets will produce a new certainty, and so on.
    let rec solve m =
      match List.tryFind withSingleMatch m with
      | Some (_, SingletonSet idx as x) -> x :: solve (List.map (fun (name, idxs) -> (name, Set.remove idx idxs)) m)
      | None -> []
      | _ -> failwith "Malformed search space"

    // Eliminate invalid tickets, then transpose the list of tickets and test
    // _columns_ of values to find where they all agree.
    let validTickets = List.filter ((invalidTicketValues program.Rules) >> Set.isEmpty) program.Nearby
    let searchSpace = [
      for (idx, ticket) in List.transpose validTickets |> List.indexed do
        for rule in program.Rules do
          if Set.isEmpty (invalidTicketValuesForRule ticket rule) then
            yield (rule.Name, idx)
    ]

    collateSet fst snd searchSpace
    |> solve 
    |> List.filter (fun (name, _) -> name.StartsWith("departure"))
    |> List.map (fun (_, idxs) -> program.Mine.[Seq.head idxs])
    |> List.reduce (*)

  let sample = parse [
    "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50";
    "your ticket:\n7,1,14";
    "nearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12";
  ]

  let sample2 = parse [
    "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19";
    "your ticket:\n11,12,13";
    "nearby tickets:\n3,9,18\n15,1,5\n5,14,9";
  ]

  let input = readNewlineGroups (inputData 2020 16) |> parse

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
