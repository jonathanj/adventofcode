namespace AdventOfCode.Year2020
module Day3 =
  open AdventOfCode.Util

  let sample = [
    "..##.......";
    "#...#...#..";
    ".#....#..#.";
    "..#.#...#.#";
    ".#...##..#.";
    "..#.##.....";
    ".#.#.#....#";
    ".#........#";
    "#.##...#...";
    "#...##....#";
    ".#..#...#.#";
  ]

  let slopes = [
    (1, 1);
    (3, 1);
    (5, 1);
    (7, 1);
    (1, 2);
  ]

  type World = (int * int * List<string>)
  type Coord = int * int

  let parse (map: List<string>) =
    (map.[0].Length, map.Length, map)

  let move ((x, y): Coord)
           ((ox, oy): Coord)
           ((w, h, map): World): option<Coord * char> =
    if (y + oy) < h then
      let (nx, ny) = ((x + ox) % w, y + oy)
      Some ((nx, ny), map.[ny].[nx])
    else
      None

  let sol1 world offset =
    let rec solve pos cnt =
      match (move pos offset world) with
        | None -> cnt
        | Some (pos, ch) -> 
            solve pos (cnt + (if ch = '#' then 1L else 0L))
    solve (0, 0) 0L

  let sol2 world slopes =
    List.fold (*) 1L (List.map (sol1 world) slopes)

  let input = List.ofSeq (readLines (inputData 2020 3))

  let main () =
    printfn "%A" (sol1 (parse input) (3, 1))
    printfn "%A" (sol2 (parse input) slopes)