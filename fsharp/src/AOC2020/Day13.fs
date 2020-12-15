namespace AdventOfCode.Year2020
module Day13 =
  open AdventOfCode.Util

  let parse (lines: list<string>) =
    match lines with
    | [targetTime; busIDs] ->
      let pair i x =
        match tryParseInt64 x with
        | Some n -> Some ((int64 i), n)
        | None -> None
      ( int64 targetTime
      , busIDs.Split(",") |> (Seq.mapi pair) |> List.ofSeq
      )
    | _ -> failwith "Unexpected input"

  let sol1 (targetTime: int64, busIDs: list<option<int64 * int64>>) =
    let frac x =
      let r = (float targetTime) / (float x)
      r - (floor r)

    let n = snd (List.maxBy (snd >> frac) (List.choose id busIDs))
    let departTime = int64 (ceil ((float targetTime) / (float n)) * (float n))
    (departTime - targetTime) * n

  let rec gcd a b =
    if b = 0L 
      then abs a
    else gcd b (a % b)

  let MI (n: int64) (g: int64): int64 =
    let rec fN (n: int64) (i: int64) (g: int64) (e: int64) (l: int64) (a: int64) =
      match e with
      | 0L -> g
      | _ -> let o = n/e
             fN e l a (n-o*e) (i-o*l) (g-o*a) 
    (n+(fN n 1L 0L g 0L 1L)) % n

  // From https://rosettacode.org/wiki/Chinese_remainder_theorem#F.23
  let CD (n: seq<int64>) (g: seq<int64>) =
    match Seq.fold (fun (n: int64) (g: int64) -> if (gcd n g) = 1L then n * g else 0L) 1L g with
    | 0L -> None
    | fN -> Some ((Seq.fold2 (fun n i g -> n + i * (fN / g) * (MI g ((fN / g) % g))) 0L n g) % fN)

  let sol2 (_, input) =
    // Basically structure the problem as target remainders and input modulos,
    // to obtain the desired output sequence:
    // CD([0, 13 - 1, …, …, 59 - 4, …, 31 - 6, 19 - 7],
    //    [7, 13    , …, …, 59    , …, 31    , 19    ]) = 1068781
    let (n, g) =
      List.choose
        (fun x ->
          match x with
          | Some (i, n) -> Some (if i = 0L then (0L, n) else (n - i, n))
          | None -> None)
        input
      |> List.unzip
    Option.get (CD n g)

  let sample = parse [
    "939";
    "7,13,x,x,59,x,31,19";
  ]

  let input =
    readLines (inputData 2020 13)
    |> List.ofSeq
    |> parse

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
