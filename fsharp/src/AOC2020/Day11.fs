namespace AdventOfCode.Year2020
module Day11 =
  open AdventOfCode.Util
  open FSharp.Collections.ParallelSeq

  type State = (string * int * int)

  let countOf x l =
    l
    |> List.filter (fun x' -> x' = x)
    |> List.length

  let parse input: State =
    let width = List.head input |> String.length
    let height = List.length input
    let world = String.concat "" input
    (world, width, height)

  let offsets = [
    (-1, -1);
    ( 0, -1);
    ( 1, -1);
    (-1,  0);
    //( 0;  0);
    ( 1,  0);
    (-1,  1);
    ( 0,  1);
    ( 1,  1);
  ]

  let lookup ((world, width, height): State) (x, y) =
    if x < 0 || y < 0 || x >= width || y >= height then
      None
    else
      Some world.[y * width + x]

  let neighbours state (x, y) =
    let cs = List.map (fun (ox, oy) -> lookup state (x + ox, y + oy)) offsets
             |> List.choose id
    (Option.get (lookup state (x, y)), cs)

  let sight state (x, y) =
    let rec see (x, y) (ox, oy) =
      let (nx, ny) = (x + ox, y + oy)
      match lookup state (nx, ny) with
      | Some ('L' | '#' as ch) -> (Some ch)
      | Some '.' -> see (nx, ny) (ox, oy)
      | None -> None
      | _ -> failwith "Unexpected value"

    let cs = List.map (see (x, y)) offsets |> List.choose id
    (Option.get (lookup state (x, y)), cs)

  let gen (state: State) f rule: State =
    let (world, width, height) = state
    let foo idx ch = 
      let (x, y) = (idx % width, idx / width)
      rule (f state (x, y))
    (String.mapi foo world, width, height)

  let solve state f rule =
    let rec loop prev n =
      match gen prev f rule with
      | next when prev = next -> next
      | next -> loop next (n + 1)

    loop state 0
    |> (fun (world, _, _) -> List.ofSeq world)
    |> countOf '#'

  let sol1 state =
    let rule x =
      match x with
      | ('L', cs) when (countOf '#' cs) = 0 -> '#'
      | ('#', cs) when (countOf '#' cs) >= 4 -> 'L'
      | (ch, _) -> ch

    solve state neighbours rule

  let sol2 state =
    let rule x =
      match x with
      | ('L', cs) when (countOf '#' cs) = 0 -> '#'
      | ('#', cs) when (countOf '#' cs) >= 5 -> 'L'
      | (ch, _) -> ch

    solve state sight rule

  let sample = parse [
    "L.LL.LL.LL";
    "LLLLLLL.LL";
    "L.L.L..L..";
    "LLLL.LL.LL";
    "L.LL.LL.LL";
    "L.LLLLL.LL";
    "..L.L.....";
    "LLLLLLLLLL";
    "L.LLLLLL.L";
    "L.LLLLL.LL";
  ]

  let input =
    readLines (inputData 2020 11)
    |> List.ofSeq
    |> parse

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
