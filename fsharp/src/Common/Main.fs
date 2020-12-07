[<EntryPoint>]
let main argv =

  let fn = match (Array.map int argv) with
           | [|2020; 1|] -> Some AdventOfCode.Year2020.Day1.main
           | [|2020; 2|] -> Some AdventOfCode.Year2020.Day2.main
           | [|2020; 3|] -> Some AdventOfCode.Year2020.Day3.main
           | [|2020; 4|] -> Some AdventOfCode.Year2020.Day4.main
           | [|2020; 5|] -> Some AdventOfCode.Year2020.Day5.main
           | [|2020; 6|] -> Some AdventOfCode.Year2020.Day6.main
           | [|2020; 7|] -> Some AdventOfCode.Year2020.Day7.main
           | _ -> None

  match fn with
    | Some fn -> fn (); 0
    | _ -> printfn "Usage: <year> <day>"; 1