namespace AdventOfCode
module Util =
  open System
  open System.Globalization
  open System.IO
  open System.Text.RegularExpressions

  let inputPath (year: int) (name: string) =
    sprintf "../inputs/%d/%s" year name

  let inputData (year: int) (day: int) =
    inputPath year (sprintf "day%d" day)

  let readLines (filePath: string) = seq {
      use sr = new StreamReader (filePath)
      while not sr.EndOfStream do
          yield sr.ReadLine()
  }

  let readFile (filePath: string) =
      use sr = new StreamReader (filePath)
      sr.ReadToEnd()

  let readNewlineGroups (filePath: string) =
      readFile filePath
      |> (fun s -> s.TrimEnd().Split("\n\n"))
      |> Seq.ofArray

  let readInts (filePath: string) =
    readLines filePath |> Seq.map int

  let readInt64s (filePath: string) =
    readLines filePath |> Seq.map int64

  let readBigInts (filePath: string) =
    readLines filePath |> Seq.map System.Numerics.BigInteger.Parse

  let (|Integer|_|) (str: string) =
     let mutable intvalue = 0
     if System.Int32.TryParse(str, &intvalue) then Some intvalue
     else None

  let (|Integer64|_|) (str: string) =
     let mutable value = 0L
     if System.Int64.TryParse(str, &value) then Some value
     else None

  let (|Character|_|) (str: string) = Seq.tryHead str

  let (|String|_|) (str: string) = Some str

  let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)
    if m.Success
    then Some (List.tail [ for x in m.Groups -> x.Value ])
    else None

  let keys (map: Map<'k, 'v>) =
    seq {
        for KeyValue(key, _) in map do
            yield key
    } |> Set.ofSeq

  let vals (map: Map<'k, 'v>) =
    seq {
        for KeyValue(_, value) in map do
            yield value
    }

  let tryParseInt (s: string) =
    try
      Some (int s)
    with :? System.FormatException -> None

  let tryParseInt64 (s: string) =
    try
      Some (System.Int64.Parse s)
    with :? System.FormatException -> None

  let between a b n =
    n >= a && n <= b

  let lines (s: string) = Seq.ofArray (s.Split("\n"))
  let linesl = lines >> List.ofSeq

  let words = Regex("\s").Split

  let binaryToInt32 (s: string) =
    System.Convert.ToInt32(s, 2) 

  let (|EmptySet|_|) a = if Set.isEmpty a then Some () else None

  let (|SingletonSet|_|) a = if (Set.count a) = 1 then Some (Seq.head a) else None

  let (|EmptyMap|_|) a = if Map.isEmpty a then Some () else None

  let replaceFirst (s: string) (a: string) (b: string) =
    Regex(a).Replace(s, b, 1)

  // https://stackoverflow.com/questions/4556160/is-there-more-simple-or-beautiful-way-to-reverse-a-string
  let reversedString str =
    let si = StringInfo(str)
    let teArr = Array.init si.LengthInTextElements (fun i -> si.SubstringByTextElements(i,1))
    Array.Reverse(teArr) //in-place reversal better performance than Array.rev
    String.Join("", teArr)

  let frequencies xs =
    List.groupBy id xs
    |> List.map (fun (k, vs) -> (k, List.length vs))
    |> Map.ofList

  let mergeMap l r =
    Map.fold (fun s k v -> Map.add k v s) l r

  let minMax xs = (List.min xs), (List.max xs)

  let rec iterate f value = seq { 
    yield value
    yield! iterate f (f value) }