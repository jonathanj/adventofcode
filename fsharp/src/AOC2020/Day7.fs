namespace AdventOfCode.Year2020
module Day7 =
  open AdventOfCode.Util

  let parse inputs =
    let parseBag (bag: string) =
      match bag with
      | ParseRegex "^(\d+) (.*?)s?\.?$" [Integer n; String color] -> Some (n, color)
      | "no other bags." -> None
      | _ -> failwith "Invalid bag description"

    let parseLine (line: string) =
      match line.Split(" contain ") with
      | [| ParseRegex "^(.*)s$" [String container]; contains |] ->
        (container, List.ofSeq <| Seq.map parseBag (contains.Split(", ")))
      | _ -> failwith "Invalid input"

    Seq.map parseLine inputs

  let sol1 inputs =
    let explode (k, vs) =
      Seq.map (fun v -> (v, k)) vs

    let bagMap =
      parse inputs
      |> Seq.map (fun (k, bags) -> (k, List.choose id bags |> List.map snd))
      |> Seq.collect explode
      |> Seq.fold (fun result (k, v) ->
                    Map.change k (fun mv -> Some (Set.add v (Option.defaultValue Set.empty mv))) result)
                  Map.empty

    let rec solve seen bags =
      match bags with
      | [] -> seen
      | (bag :: toVisit) ->
        match (bagMap.TryFind bag, toVisit) with
        | (None, []) | (Some EmptySet, []) -> Set.add bag seen
        | (None, toVisit) -> solve (Set.add bag seen) toVisit
        | (Some nextToVisit, toVisit) -> solve (Set.add bag seen) (toVisit @ List.ofSeq nextToVisit)

    solve Set.empty ["shiny gold bag"]
    |> Set.remove "shiny gold bag"
    |> Set.count

  let sol2 inputs =
    let bagMap = Map.ofSeq (parse inputs)
    let rec bagCost name =
      match bagMap.TryFind name with
      | Some next  ->
          List.choose id next
          |> List.fold (fun result (n, name) -> result + (n * (bagCost name))) 1
      | None -> 0

    (bagCost "shiny gold bag") - 1

  let sample = Seq.ofList [
    "light red bags contain 1 bright white bag, 2 muted yellow bags.";
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.";
    "bright white bags contain 1 shiny gold bag.";
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.";
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.";
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.";
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.";
    "faded blue bags contain no other bags.";
    "dotted black bags contain no other bags.";
  ]
  let sample2 = Seq.ofList [
    "shiny gold bags contain 2 dark red bags.";
    "dark red bags contain 2 dark orange bags.";
    "dark orange bags contain 2 dark yellow bags.";
    "dark yellow bags contain 2 dark green bags.";
    "dark green bags contain 2 dark blue bags.";
    "dark blue bags contain 2 dark violet bags.";
    "dark violet bags contain no other bags.";
  ]
  let input = readLines (inputData 2020 7)

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
