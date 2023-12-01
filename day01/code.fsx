#r "nuget: FParsec"
open FParsec

let pdigit = (skipMany letter) >>. digit .>> (skipMany letter)

let getDigits p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure _ -> [ '0' ]

let getFirstAndLastDigit (xs: char list) =
    match xs with
    | [] -> ('0', '0')
    | [ only ] -> (only, only)
    | [ first; last ] -> (first, last)
    | first :: rest -> (first, List.last rest)

let num (d: char) = int d - int '0'

let toNumber (x: char, y: char) = num x * 10 + num y

let input = System.IO.File.ReadLines("input.txt")

let printAndForward x =
    printfn "%A" x
    x

input
|> Seq.map printAndForward
|> Seq.map (getDigits (many pdigit))
|> Seq.map printAndForward
|> Seq.map getFirstAndLastDigit
|> Seq.map printAndForward
|> Seq.map toNumber
|> Seq.map printAndForward
|> Seq.sum
