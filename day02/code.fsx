#r "nuget: FParsec"
open FParsec

let str s = pstring s
let comma: Parser<string, unit> = str ","
let semicolon: Parser<string, unit> = str ";"
let game: Parser<(string * int32), unit> = str "Game " .>>. pint32 .>> str ":"
let number: Parser<int32, unit> = spaces >>. pint32
let redCube: Parser<string, unit> = spaces >>. str "red"
let greenCube: Parser<string, unit> = spaces >>. str "green"
let blueCube: Parser<string, unit> = spaces >>. str "blue"
let numberOfRedCube: Parser<(int32 * string), unit> = number .>>. redCube
let numberOfGreenCube: Parser<(int32 * string), unit> = number .>>. greenCube
let numberOfBlueCube: Parser<(int32 * string), unit> = number .>>. blueCube

let numberOfCube: Parser<(int32 * string), unit> =
    ((attempt numberOfRedCube)
     <|> (attempt numberOfGreenCube)
     <|> (attempt numberOfBlueCube))

let cubeSet: Parser<list<int32 * string>, unit> = sepBy numberOfCube comma
let gameSet = game .>>. (sepBy cubeSet semicolon) .>> newline

let ast =
    (runParserOnFile (many gameSet) () "day02/input.txt" (System.Text.UTF8Encoding())
     |> function
         | Success (result, _, _) -> result
         | Failure (_) -> failwith "Unable to Parse the file")

let folder (state: Map<string, int>) ((k, v): (string * int)) =
    if not (state.ContainsKey k) then
        state.Add(k, v)
    else if state.[k] <= v then
        state.Add(k, v)
    else
        state

let toMap setList =
    setList
    |> List.concat
    |> List.map (fun (v, k) -> (k, v))
    |> List.fold folder Map.empty

let isHigherThan key value map =
    Map.tryFind key map |> Option.defaultValue 0 > value

let isMoreThanAvailableRed = isHigherThan "red" 12
let isMoreThanAvailableGreen = isHigherThan "green" 13
let isMoreThanAvailableBlue = isHigherThan "blue" 14

let predicate (key: int, (m: Map<string, int>)) =
    if isMoreThanAvailableRed m
       || isMoreThanAvailableGreen m
       || isMoreThanAvailableBlue m then
        0
    else
        key

// Part One
ast
|> List.map (fun ((_, i), setList) -> (i, setList |> toMap))
|> List.map predicate
|> List.sum

// Part Two
let power (key: int, (m: Map<string, int>)) =
    m
    |> Map.toList
    |> List.map (fun (k, v) -> v)
    |> List.reduce (*)

ast
|> List.map (fun ((_, i), setList) -> (i, setList |> toMap))
|> List.map power
|> List.sum
