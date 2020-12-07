namespace AdventOfCode
module Util =
  open System.IO
  open System.Text.RegularExpressions

  let inputData (year: int) (day: int) =
    sprintf "../inputs/%d/day%d" year day

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

  let readInts (filePath: string) = readLines filePath |> Seq.map System.Int32.Parse

  let (|Integer|_|) (str: string) =
     let mutable intvalue = 0
     if System.Int32.TryParse(str, &intvalue) then Some intvalue
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
        for KeyValue(key,value) in map do
            yield key
    } |> Set.ofSeq

  let tryParseInt (s: string) =
    try
      Some (int s)
    with :? System.FormatException -> None

  let between a b n =
    n >= a && n <= b

  let lines (s: string) = Seq.ofArray (s.Split("\n"))

  let words = Regex("\s").Split

  let binaryToInt32 (s: string) =
    System.Convert.ToInt32(s, 2) 

  let (|EmptySet|_|) a = if Set.isEmpty a then Some () else None