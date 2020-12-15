namespace AdventOfCode.Year2020
module Day10 =
  open AdventOfCode.Util

  let frequencies xs =
    List.groupBy id xs
    |> List.map (fun (k, vs) -> (k, List.length vs))
    |> Map.ofList

  let sol1 input =
    let xs = List.sort input
    let freqs: Map<int, int> =
      List.map
        (fun (a, b) -> b - a)
        (List.pairwise ([0] @ (List.sort xs) @ [(List.max xs) + 3]))
        |> frequencies

    freqs.[1] * freqs.[3]

  let rec span a b =
    match a, b with 
    | _, (  []     as xs) -> xs, xs
    | p, ((x::xs') as xs) ->
            if  p x then let ys, zs = span p xs' in (x::ys, zs)
            else [], xs

  let sol2 input =
    let xs = List.sort input
    let diffs =
      List.map
        (fun (a, b) -> b - a)
        (List.pairwise (0::(List.sort xs)))

    let combos xs =
      match (List.length xs) + 1 with
      | 5 -> 7L
      | 4 -> 4L
      | 3 -> 2L
      | _ -> 1L

    let rec solve result xs =
      match span ((=) 1) xs with
      | (matched, []) -> (combos matched) :: result
      | (matched, _::xs) -> (combos matched) :: result @ (solve result xs)

    (printfn "%A" diffs)
    (printfn "%A" (solve [] diffs))
    List.reduce (fun a b -> a * b) (solve [] diffs)

  let sample = [
    16; 10; 15; 5; 1; 11; 7; 19; 6; 12; 4; ]
  let sample2 = [
    28; 33; 18; 42; 31; 14; 46; 20; 48; 47; 24; 23; 49; 45; 19; 38; 39; 11; 1;
    32; 25; 35; 8; 17; 7; 9; 4; 2; 34; 10; 3; ]
  let input = List.ofSeq <| readInts (inputData 2020 10)

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
