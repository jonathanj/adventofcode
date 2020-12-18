namespace AdventOfCode.Year2020
module Day18 =
  open AdventOfCode.Util
  open FParsec

  let makeParser ops =
    let str_ws s = pstring s >>. spaces
    let opp = new OperatorPrecedenceParser<int64,unit,unit>()
    let expr = opp.ExpressionParser
    let term = (pint64 .>> spaces) <|> between (str_ws "(") (str_ws ")") expr
    opp.TermParser <- term
    for op in ops do
      opp.AddOperator(op)
    opp.ExpressionParser

  let parse parser input =
    match run parser input with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg

  let sol1 exprs =
    let parser = makeParser [
      InfixOperator("+", spaces, 1, Associativity.Left, (+))
      InfixOperator("*", spaces, 1, Associativity.Left, (*))
    ]
    Seq.sumBy (parse parser) exprs

  let sol2 exprs =
    let parser = makeParser [
      InfixOperator("+", spaces, 2, Associativity.Left, (+))
      InfixOperator("*", spaces, 1, Associativity.Left, (*))
    ]
    Seq.sumBy (parse parser) exprs

  let sample = ["1 + 2 * 3 + 4 * 5 + 6"]
  let sample2 = ["1 + (2 * 3) + (4 * (5 + 6))"]
  let sample3 = ["2 * 3 + (4 * 5)"]

  let input = readLines (inputData 2020 18)

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
