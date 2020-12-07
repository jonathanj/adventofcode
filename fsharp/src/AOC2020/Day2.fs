namespace AdventOfCode.Year2020
module Day2 =
  open AdventOfCode.Util

  type ParsedPassword = int * int * char * string

  let parse (line: string): option<ParsedPassword> =
    match line with
      | ParseRegex "^(\d+)-(\d+) (.): (.*)$" [Integer m; Integer n; Character c; String p]
        -> Some (m, n, c, p)
      | _ -> None

  let countOf s x =
    s
    |> Seq.filter (fun x' -> x' = x)
    |> Seq.length

  let isValidCounts ((m, n, c, p): ParsedPassword) =
    let count = countOf p c
    count >= m && count <= n

  let isValidOccurence ((m, n, c, p): ParsedPassword) =
    let a = p.[n - 1]
    let b = p.[m - 1]
    a <> b && (a = c || b = c)

  let sample = [
    "1-3 a: abcde";
    "1-3 b: cdefg";
    "2-9 c: ccccccccc";
  ]

  let input =
    readLines (inputData 2020 2)
    |> Seq.map parse
    |> Seq.choose id

  let main () =
    printfn "%A" ((Seq.filter isValidCounts input) |> Seq.length)
    printfn "%A" ((Seq.filter isValidOccurence input) |> Seq.length)