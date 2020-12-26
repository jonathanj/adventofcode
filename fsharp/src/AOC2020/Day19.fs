namespace AdventOfCode.Year2020
module Day19 =
  open AdventOfCode.Util

  type Chars = list<char>
  type RuleId = int
  type Rule =
    | Sequence of list<RuleId>
    | Literal of char
    | Disjunction of list<RuleId> * list<RuleId>
  type Rules = Map<int, Rule>

  let ints xs = Seq.map int xs |> List.ofSeq

  let parseRule (rule: string) =
    match rule.Split(": ") with
    | [| n; rule |] ->
      let r =
        match rule.Split(" | ") with
        | [| a; b |] -> Disjunction ((ints (words a)), (ints (words b)))
        | [| a |] ->
            match a with
            | ParseRegex "\"([a-z])\"" [Character l] -> Literal l
            | _ -> Sequence (ints (words a))
        | _ -> failwith "Malformed rule"
      (int n, r)
    | _ -> failwith "Malformed rule"

  let parse input: Rules * list<Chars> =
    match input with
    | [rules; inputs] ->
        let rules = List.map parseRule (linesl rules) |> Map.ofList
        (rules, List.map List.ofSeq (linesl inputs))
    | _ -> failwith "Malformed input"

  let execute (rules: Rules) (input: list<Chars>) =
    let rec matches (rule: Rule) (input: Chars): option<list<Chars>> =
      match (rule, input) with
      | (Literal _), [] -> None
      | (Literal ch), (x::xs) -> if x = ch then Some [xs] else None
      | (Sequence []), input -> Some [input]
      | (Sequence (x::xs)), input ->
          matches rules.[x] input
          |> Option.map (List.choose (matches (Sequence xs)) >> List.collect id)
      | (Disjunction (l, r)), input ->
        match (matches (Sequence l) input, matches (Sequence r) input) with
        | (Some a), (Some b) -> Some (List.append a b)
        | (Some a), None     -> Some a
        | None,     (Some b) -> Some b
        | _                  -> None

    let isValid = function
    | Some x -> List.contains [] x
    | None -> false

    List.map (matches rules.[0]) input
    |> List.filter isValid

  let sol1 (rules: Rules, inputs: list<Chars>) =
    execute rules inputs
    |> List.length

  let sol2 (rules: Rules, inputs: list<Chars>) =
    let rule8 = "8: 42 | 42 8"
    let rule11 = "11: 42 31 | 42 11 31"
    let newRules = mergeMap rules (Map.ofList (List.map parseRule [rule8; rule11]))
    sol1 (newRules, inputs)

  let sample = parse [
    "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"";
    "ababbb\nbababa\nabbbab\naaabbb\naaaabbb";
  ]

  let sample2 = readNewlineGroups (sprintf "../inputs/2020/day19_sample") |> List.ofSeq |> parse

  let input = readNewlineGroups (inputData 2020 19) |> List.ofSeq |> parse

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
