#r "nuget: FParsec"
open FParsec

// module parser =
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

let input =
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"

run (many gameSet) input

runParserOnFile (many gameSet) () "day02/input.txt" (System.Text.UTF8Encoding())
