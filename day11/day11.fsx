open System.IO
open System.Text.RegularExpressions
open System

type Monkey =
    { Id: uint64
      Items: uint64 list
      Operation: uint64 -> uint64
      Test: uint64
      IfTrue: uint64
      IfFalse: uint64
      NumInspected: uint64 }

let parseItems (line: string) = line.Split ", " |> Array.map uint64

let parseOperation (line: string) (old: uint64) =
    let ops = line.Replace("old", string old).Split(" ")

    let op =
        match ops[1] with
        | "*" -> (*)
        | "+" -> (+)

    op (uint64 ops[0]) (uint64 ops[2])

let parseMonkey text =
    let regex =
        "Monkey (\d+):\
        \W*Starting items: (.+)\
        \W*Operation: new = (.+)\
        \W*Test: divisible by (\d+)\
        \W*If true: throw to monkey (\d+)\
        \W*If false: throw to monkey (\d+)"

    let m = Regex(regex, RegexOptions.Multiline).Match(text)
    let items = parseItems m.Groups[2].Value |> Array.toList |> List.map uint64
    let g = m.Groups

    { Id = uint64 g[1].Value
      Items = items
      Operation = parseOperation g[3].Value
      Test = uint64 g[4].Value |> uint64
      IfTrue = uint64 g[5].Value
      IfFalse = uint64 g[6].Value
      NumInspected = uint64 0 }


let evalMonkey (monkeys: Monkey[]) monkey part2 =
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
        |> List.map (fun item ->
            if part2 then
                monkey.Operation item % ((1UL, monkeys) ||> Array.fold (fun acc m -> m.Test * acc))
            else
                monkey.Operation item / 3UL)
        |> List.map (fun item ->
            if item % monkey.Test = 0UL then
                monkey.IfTrue, item
            else
                monkey.IfFalse, item)
        |> List.groupBy fst
        |> List.map (fun (key, ls) -> key, ls |> List.map snd)
        |> Map

    newMonkeys
    |> Array.mapi (fun i m ->
        if throwTo.ContainsKey(uint64 i) then
            { Id = m.Id
              Items = m.Items @ throwTo[uint64 i]
              Operation = m.Operation
              Test = m.Test
              IfTrue = m.IfTrue
              IfFalse = m.IfFalse
              NumInspected = m.NumInspected }
        else
            m)


let playRound (monkeys: Monkey[]) part2 =
    (monkeys, [ 0 .. monkeys.Length - 1 ])
    ||> List.fold (fun mnks i -> evalMonkey mnks mnks[i] part2)

let playGame monkeys nRounds part2 =
    let moves = (monkeys, [ 1..nRounds ]) ||> List.fold (fun m _ -> playRound m part2)
    let numInspected = moves |> Array.map (fun m -> m.NumInspected)
    numInspected |> Array.sort |> Array.rev |> Array.take 2 |> Array.reduce (*)

let text = File.ReadAllText("input.txt")

let lines =
    text.Split(String.concat "" [ Environment.NewLine; Environment.NewLine ])

let monkeys = lines |> Array.map parseMonkey

let part1 = playGame monkeys 20 false
printfn $"Question 1: {part1}"

let part2 = playGame monkeys 10000 true
printfn $"Question 2: {part2}"
