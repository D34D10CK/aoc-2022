open System.IO

let lines = File.ReadAllLines("input1.txt")

let sumElf acc curr =
    match curr with
    | "" -> Array.append acc [| 0 |]
    | x ->
        match acc with
        | [||] -> [| x |> int |]
        | _ -> Array.append acc[.. acc.Length - 2] [| acc[acc.Length - 1] + (x |> int) |]

let elves = ([||], lines) ||> Array.fold sumElf
let sorted = elves |> Array.sortBy (fun x -> -x - 1)

printfn $"Question 1: {sorted[0]}"

let top3Sum = sorted[..2] |> Array.sum
printfn $"Question 2: {top3Sum}"
