namespace AdventOfCode.Year2020
module Day14 =
  open System.Text.RegularExpressions
  open AdventOfCode.Util

  type State =
    { Mask: string;
      Registers: Map<string, int64> }

  let solve input setAddress =
    let execute (state: State) (s: string) =
      match s with
      | ParseRegex "mask = ([X10]+)" [String mask] ->
        { state with Mask = mask }
      | ParseRegex "mem\[(\d+)\] = (\d+)" [String addr; Integer64 value] ->
        setAddress state addr value
      | _ -> failwithf "Unknown input: %A" s

    let finalState = Seq.fold execute { Mask = "".PadLeft(36, '0'); Registers = Map.empty } input
    Map.fold (fun result _ value -> result + value) 0L finalState.Registers

  let sol1 input =
    let setAddress state addr value =
      let _and = System.Convert.ToInt64(Regex("[^0]").Replace(state.Mask, "1"), 2)
      let _or = System.Convert.ToInt64(Regex("[^1]").Replace(state.Mask, "0"), 2)
      { state with Registers = Map.add addr ((value &&& _and) ||| _or) state.Registers }

    solve input setAddress

  let sol2 input =
    let applyMask (addr: string) (mask: string) =
      let addrBinary = System.Convert.ToString(int addr, 2).PadLeft(36, '0')
      List.fold2
        (fun result a b ->
          match a with
          | 'X' -> result + "X"
          | '0' -> result + (string b)
          | '1' -> result + "1"
          | _   -> failwith "Unexpected input")
        ""
        (List.ofSeq mask)
        (List.ofSeq addrBinary)

    // Recursively generate both 0 and 1 replacements for every X, to cover every combination.
    let rec combinations (s: string) =
      if s.Contains("X") then
        let s0 = replaceFirst s "X" "0"
        let s1 = replaceFirst s "X" "1"
        combinations s0 @ combinations s1
      else
        [s]

    let setAddress state addr value =
      List.fold
        (fun state addr -> { state with Registers = Map.add addr value state.Registers })
        state
        (combinations (applyMask addr state.Mask))

    solve input setAddress

  let sample = [
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X";
    "mem[8] = 11";
    "mem[7] = 101";
    "mem[8] = 0";
  ]

  let sample2 = [
    "mask = 000000000000000000000000000000X1001X";
    "mem[42] = 100";
    "mask = 00000000000000000000000000000000X0XX";
    "mem[26] = 1";
  ]

  let input = readLines (inputData 2020 14)

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
