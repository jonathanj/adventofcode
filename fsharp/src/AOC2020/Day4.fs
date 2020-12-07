namespace AdventOfCode.Year2020
module Day4 =
  open AdventOfCode.Util

  let sample = @"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
  byr:1937 iyr:2017 cid:147 hgt:183cm

  iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
  hcl:#cfa07d byr:1929

  hcl:#ae17e1 iyr:2013
  eyr:2024
  ecl:brn pid:760753108 byr:1931
  hgt:179cm

  hcl:#cfa07d eyr:2025 pid:166559648
  iyr:2011 ecl:brn hgt:59in"

  let parse (input: string) =
    let kvPair (s: string) =
      match s.Split(":") with
        | [| k; v |] -> Some (k, v)
        | _ -> None

    input.Split "\n\n"
    |> Array.map (words >> Array.map kvPair >> Array.choose id >> Map.ofArray)
    |> List.ofArray

  let hasRequiredFields (fields: Map<string, string>) =
    (keys fields).IsSupersetOf (Set ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"])

  type Height =
    | Cm of n: int
    | In of n: int

  let passportHeight (str: string) =
    match str with
      | ParseRegex "^(\d+)(cm|in)$" [Integer value; String unt] ->
        match unt with
          | "cm" -> Some (Cm value)
          | "in" -> Some (In value)
          | _ -> None
      | _ -> None

  let isValidPassport (fields: Map<string, string>) =
    let x = tryParseInt "1234" |> Option.exists (between 1920 2002)

    Map.forall (fun key value ->
      match key with
        | "byr" -> tryParseInt value |> Option.exists (between 1920 2002)
        | "iyr" -> tryParseInt value |> Option.exists (between 2010 2020)
        | "eyr" -> tryParseInt value |> Option.exists (between 2020 2030)
        | "hgt" -> match passportHeight value with
                     | Some (Cm n) -> between 150 193 n
                     | Some (In n) -> between 59 76 n
                     | _ -> false
        | "hcl" -> match value with
                     | ParseRegex "^#([0-9a-f]{6})$" [String _] -> true
                     | _ -> false
        | "ecl" -> match value with
                     | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
                     | _ -> false
        | "pid" -> match value with
                     | ParseRegex "^(\d{9})$" [String _] -> true
                     | _ -> false
        | "cid" -> true
        | _ -> false
    ) fields

  let sol1 input =
    List.filter hasRequiredFields input |> List.length

  let sol2 input =
    List.filter hasRequiredFields input
    |> List.filter isValidPassport
    |> List.length

  let input = parse (readFile (inputData 2020 4))

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)