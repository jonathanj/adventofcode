namespace AdventOfCode.Year2020
module Day23 =
  open AdventOfCode.Util

  type State = int * int[]

  // Build an array mapping each value (stored as value - 1 because of
  // zero-based indexing) to the next numbers clockwise, with the last number
  // wrapping around. Arrays are significantly faster for the changes required
  // because they're mutable.
  let buildLinks (numbers: seq<int>) =
    let first = Seq.head numbers
    let links =
      Seq.fold2
        (fun (links: int[]) a b ->
          links.[a - 1] <- b
          links)
        (Array.zeroCreate (Seq.length numbers))
        numbers
        (Seq.append (Seq.tail numbers) [first])
    (first, links)

  let parse (input: string): State =
    buildLinks (Seq.map (string >> int) input)

  let rec round ((current, links): State) =
    let a = links.[current - 1]
    let b = links.[a - 1]
    let c = links.[b - 1]

    let rec findDestination dest =
      if (dest = a || dest = b || dest = c) then
        findDestination (dest - 1)
      elif dest < 1 then
        findDestination (Array.length links)
      else
        dest

    let dest = findDestination (current - 1)
    let newCurrent = links.[c - 1]
    links.[c - 1] <- links.[dest - 1]
    links.[dest - 1] <- a
    links.[current - 1] <- newCurrent
    (newCurrent, links)

  let solve state numRounds =
    let mutable result = state
    for _ = 1 to numRounds do
      result <- round result
    result

  let sol1 state =
    let stateToString ((_, links): State) =
      let mutable next = links.[0]
      let mutable result = [string next]
      for _ = 0 to 6 do
        result <- result @ [string links.[next - 1]]
        next <- links.[next - 1]
      String.concat "" result

    solve state 100 |> stateToString

  let sol2 input =
    let numbers = Seq.append (Seq.map (string >> int) input) [10..1_000_000]
    let (_, links) = solve (buildLinks numbers) 10_000_000
    let a = links.[0]
    let b = links.[a - 1]
    (int64 a) * (int64 b)

  let sample = "389125467"

  let input = readLines (inputData 2020 23) |> Seq.head

  let main () =
    printfn "%s" (sol1 (parse input))
    printfn "%d" (sol2 input)
