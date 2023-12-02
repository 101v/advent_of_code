#r "nuget: FParsec"
open FParsec

let digitMap =
    [ ("one", 1)
      ("two", 2)
      ("three", 3)
      ("four", 4)
      ("five", 5)
      ("six", 6)
      ("seven", 7)
      ("eight", 8)
      ("nine", 9)
      ("zero", 0)
      ("0", 0)
      ("1", 1)
      ("2", 2)
      ("3", 3)
      ("4", 4)
      ("5", 5)
      ("6", 6)
      ("7", 7)
      ("8", 8)
      ("9", 9) ]
    |> Map

let digitParsers = digitMap.Keys |> Seq.map pstring |> choice

let mixedupDigitMap =
    [ ("oneight", (1, 8))
      ("twone", (2, 1))
      ("threeight", (3, 8))
      ("fiveight", (5, 8))
      ("sevenine", (7, 9))
      ("eightwo", (8, 2))
      ("eighthree", (8, 3))
      ("nineight", (9, 8)) ]
    |> Map

let mixedupDigitParsers = mixedupDigitMap.Keys |> Seq.map pstring |> choice

let skipOneChar = anyString 1 >>. preturn "x"

let digits =
    (many (
        mixedupDigitParsers
        <|> digitParsers
        <|> skipOneChar
    ))

let getNumbers p str =
    match run p str with
    | Success (result, _, _) ->
        result
        |> List.filter (fun x -> not (x.Equals("x")))
    | Failure _ -> [ "0" ]

let getFirstAndLastDigits xs =
    match xs with
    | [] -> ("0", "0")
    | [ only ] -> (only, only)
    | [ first; last ] -> (first, last)
    | first :: rest -> (first, List.last rest)

let convert x =
    if mixedupDigitMap.ContainsKey x then
        mixedupDigitMap.Item x
    else
        let digit = digitMap.Item x
        digit, digit

let onesDigit x = snd (convert x)

let tensDigit x = fst (convert x)
let toNumber (x, y) = (tensDigit x) * 10 + (onesDigit y)

let input = System.IO.File.ReadLines("input.txt")
// let input =
//     [ "two1nine"
//       "eightwothree"
//       "abcone2threexyz"
//       "xtwone3four"
//       "4nineeightseven2"
//       "zoneight234"
//       "7pqrstsixteen" ]

let printAndForward x =
    printfn "%A" x
    x

input
|> Seq.map printAndForward
|> Seq.map (getNumbers digits)
|> Seq.map printAndForward
|> Seq.map getFirstAndLastDigits
|> Seq.map printAndForward
|> Seq.map toNumber
|> Seq.map printAndForward
// |> Seq.take 20
// |> Seq.toList
|> Seq.sum
