open System.IO
open System.Text.RegularExpressions

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let parseLine ((fs: Map<string list, int>), (currPath: string list)) (line: string) =
    match line with
    | ParseRegex @"^\$ cd (\w+)$" [ dir ] ->
        let newPath = dir :: currPath
        fs.Add(newPath, 0), newPath
    | "$ cd .." -> fs, currPath.Tail
    | ParseRegex @"^(\d+).*" [ size ] -> fs.Change(currPath, (fun x -> Some(int size + x.Value))), currPath
    | _ -> fs, currPath

let lines = File.ReadAllLines("input.txt") |> Array.toList

let fs =
    ((Map [ ([ "/" ], 0) ], [ "/" ]), lines.Tail) ||> List.fold parseLine |> fst

let fsList =
    fs |> Map.toList |> List.map (fun x -> fst x |> String.concat "/", snd x)

let sizes =
    fsList
    |> List.map (fun (x1, _) -> fsList |> List.filter (fun (x2, _) -> x2.EndsWith(x1)) |> List.sumBy snd)

let part1 = sizes |> List.filter (fun x -> x <= 100000) |> List.sum
printfn $"Question 1: {part1}"

let freeSpace = 70000000 - List.max sizes
let dirSize = sizes |> List.filter (fun x -> x > (30000000 - freeSpace)) |> List.min
printfn $"Question 2: {dirSize}"
