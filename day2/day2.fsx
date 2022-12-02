open System.IO

type outcome =
    | Win
    | Loss
    | Draw

let parseOutcome input =
    match input with
    | 'X' -> Loss
    | 'Y' -> Draw
    | 'Z' -> Win

type move =
    | Rock
    | Paper
    | Scissors

let parseMove input =
    match input with
    | 'A'
    | 'X' -> Rock
    | 'B'
    | 'Y' -> Paper
    | 'C'
    | 'Z' -> Scissors

let winningMove play =
    match play with
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock

let losingMove play =
    match play with
    | Rock -> Scissors
    | Paper -> Rock
    | Scissors -> Paper

let score you opponent =
    let initialScore =
        match you with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let winScore =
        match you, opponent with
        | Rock, Paper -> 0
        | Scissors, Rock -> 0
        | Paper, Scissors -> 0
        | x, y when x = y -> 3
        | _ -> 6

    initialScore + winScore

let selectMove opponent target =
    match target with
    | Win -> winningMove opponent
    | Loss -> losingMove opponent
    | Draw -> opponent

let playGame (line: string) =
    let you = parseMove line[2]
    let opponent = parseMove line[0]
    score you opponent

let playGame2 (line: string) =
    let target = parseOutcome line[2]
    let opponent = parseMove line[0]
    let selectedMove = selectMove opponent target
    score selectedMove opponent

let lines = File.ReadAllLines("input.txt")
let totalScore = lines |> Array.map playGame |> Array.sum
printfn $"Question 1: {totalScore}"

let totalScore2 = lines |> Array.map playGame2 |> Array.sum
printfn $"Question 2: {totalScore2}"
