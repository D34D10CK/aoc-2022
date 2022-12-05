open System.Collections
open System.IO
open System.Text.RegularExpressions

type Move = { From: int; To: int; Amount: int }

let parseMove line =
    let regex = @"move (\d+) from (\d) to (\d)"
    let groups = Regex.Match(line, regex).Groups

    { Amount = int groups[1].Value
      From = int groups[2].Value - 1
      To = int groups[3].Value - 1 }

let executeMove (stacks: Stack[]) move =
    for _ in 1 .. move.Amount do
        let item = stacks[ move.From ].Pop()
        stacks[ move.To ].Push(item)

let executeMove2 (stacks: Stack[]) move =
    let items =
        [ for _ in 1 .. move.Amount do
              stacks[ move.From ].Pop() ]

    for item in List.rev items do
        stacks[ move.To ].Push(item)


let stacks =
    [| Stack([| 'N'; 'S'; 'D'; 'C'; 'V'; 'Q'; 'T' |])
       Stack([| 'M'; 'F'; 'V' |])
       Stack([| 'F'; 'Q'; 'W'; 'D'; 'P'; 'N'; 'H'; 'M' |])
       Stack([| 'D'; 'Q'; 'R'; 'T'; 'F' |])
       Stack([| 'R'; 'F'; 'M'; 'N'; 'Q'; 'H'; 'V'; 'B' |])
       Stack([| 'C'; 'F'; 'G'; 'N'; 'P'; 'W'; 'Q' |])
       Stack([| 'W'; 'F'; 'R'; 'L'; 'C'; 'T' |])
       Stack([| 'T'; 'Z'; 'N'; 'S' |])
       Stack([| 'M'; 'S'; 'D'; 'J'; 'R'; 'Q'; 'H'; 'N' |]) |]

let lines = File.ReadAllLines("input.txt")

let stacks1 = stacks |> Array.map (fun x -> x.Clone() :?> Stack)
lines |> Array.iter (parseMove >> executeMove stacks1)
let top = stacks1 |> Array.map (fun x -> x.Peek().ToString()) |> String.concat ""
printfn $"Question 1: {top}"

let stacks2 = stacks |> Array.map (fun x -> x.Clone() :?> Stack)
lines |> Array.iter (parseMove >> executeMove2 stacks2)
let top2 = stacks2 |> Array.map (fun x -> x.Peek().ToString()) |> String.concat ""
printfn $"Question 2: {top2}"
