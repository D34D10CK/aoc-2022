open System
open System.IO
open System.Text.RegularExpressions

let isInt s =
    try
        s |> int |> ignore
        true
    with :? FormatException ->
        false

let tokenize line =
    Regex.Split(line, @"([\[\],])")
    |> Array.filter (fun x -> x <> "," && x <> "")
    |> Array.toList

let rec compare (left: string list) (right: string list) =
    match (left, right) with
    | l :: ls, r :: rs when l = r -> compare ls rs
    | l :: _, r :: _ when (isInt l) && (isInt r) -> if int l < int r then -1 else 1
    | "]" :: _, _ -> -1
    | _, "]" :: _ -> 1
    | "[" :: ls, rs
    | ls, "[" :: rs -> compare ls rs

let text = File.ReadAllText("input.txt")

let inputs =
    text.Split(String.Concat([ Environment.NewLine; Environment.NewLine ]))
    |> Array.map (fun str ->
        let splits = str.Split(Environment.NewLine)
        (splits[0] |> tokenize, splits[1] |> tokenize))

let part1 =
    inputs
    |> Array.map (fun x -> x ||> compare)
    |> Array.mapi (fun i x -> if x = -1 then i + 1 else 0)
    |> Array.sum

printfn $"Question 1: {part1}"

let lines =
    File.ReadAllLines("input.txt")
    |> Array.filter (fun s -> not (String.IsNullOrWhiteSpace s))


let sorted =
    Array.concat [ lines; [| "[[2]]"; "[[6]]" |] ]
    |> Array.sortWith (fun el1 el2 -> compare (tokenize el1) (tokenize el2))

let idx2 = Array.findIndex ((=) "[[2]]") sorted + 1
let idx6 = Array.findIndex ((=) "[[6]]") sorted + 1

printfn $"Question 2: {idx2 * idx6}"
