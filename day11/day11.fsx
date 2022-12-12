open System.IO
open System.Text.RegularExpressions
open System

type Monkey =
    { Id: int
      Items: int list
      Operation: int -> int
      Test: int
      IfTrue: int
      IfFalse: int
      NumInspected: uint64 }

let parseItems (line: string) = line.Split ", " |> Array.map int

let parseOperation (line: string) (old: int) =
    let ops = line.Replace("old", string old).Split(" ")

    let op =
        match ops[1] with
        | "*" -> (*)
        | "+" -> (+)

    op (int ops[0]) (int ops[2])

let parseMonkey text =
    let regex =
        "Monkey (\d+):\
        \W*Starting items: (.+)\
        \W*Operation: new = (.+)\
        \W*Test: divisible by (\d+)\
        \W*If true: throw to monkey (\d+)\
        \W*If false: throw to monkey (\d+)"

    let m = Regex(regex, RegexOptions.Multiline).Match(text)
    let items = parseItems m.Groups[2].Value |> Array.toList
    let g = m.Groups

    { Id = int g[1].Value
      Items = items
      Operation = parseOperation g[3].Value
      Test = int g[4].Value
      IfTrue = int g[5].Value
      IfFalse = int g[6].Value
      NumInspected = uint64 0 }


let evalMonkey (monkeys: Monkey[]) (monkey: Monkey) =
    let newMonkeys =
        monkeys
        |> Array.mapi (fun i m ->
            if m.Id = monkey.Id then
                { Id = m.Id
                  Items = []
                  Operation = m.Operation
                  Test = m.Test
                  IfFalse = m.IfFalse
                  IfTrue = m.IfTrue
                  NumInspected = m.NumInspected + uint64 monkey.Items.Length }
            else
                m)

    let throwTo =
        monkey.Items
        |> List.map (fun item -> monkey.Operation item / 3)
        |> List.map (fun item ->
            if item % monkey.Test = 0 then
                monkey.IfTrue, item
            else
                monkey.IfFalse, item)
        |> List.groupBy fst
        |> List.map (fun (key, ls) -> key, ls |> List.map snd)
        |> Map

    newMonkeys
    |> Array.mapi (fun i m ->
        if throwTo.ContainsKey i then
            { Id = m.Id
              Items = m.Items @ throwTo[i]
              Operation = m.Operation
              Test = m.Test
              IfTrue = m.IfTrue
              IfFalse = m.IfFalse
              NumInspected = m.NumInspected }
        else
            m)


let playRound (monkeys: Monkey[]) =
    (monkeys, [ 0 .. monkeys.Length - 1 ])
    ||> List.fold (fun mnks i -> evalMonkey mnks mnks[i])

let text = File.ReadAllText("input.txt")

let lines =
    text.Split(String.concat "" [ Environment.NewLine; Environment.NewLine ])

let monkeys = lines |> Array.map parseMonkey

let moves = (monkeys, [ 1..20 ]) ||> List.fold (fun m _ -> playRound m)
let numInspected = moves |> Array.map (fun m -> m.NumInspected)

let part1 =
    numInspected |> Array.sort |> Array.rev |> Array.take 2 |> Array.reduce (*)

printfn $"Question 1: {part1}"
