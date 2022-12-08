open System.IO
open System.Text.RegularExpressions

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let parseLine ((fs: Map<string, int>), currPath) line =
    match line with
    | ParseRegex @"^\$ cd (\w+)$" [ dir ] ->
        let newPath = Path.Join(currPath, dir)
        fs.Add(newPath, 0), newPath
    | "$ cd .." -> fs, Path.GetDirectoryName(currPath)
    | ParseRegex @"^(\d+).*" [ size ] -> fs.Change(currPath, (fun x -> Some(int size + x.Value))), currPath
    | _ -> fs, currPath

let lines = File.ReadAllLines("input.txt") |> Array.toList

let rootFolder = string Path.DirectorySeparatorChar

let fs =
    ((Map [ (rootFolder, 0) ], rootFolder), lines.Tail)
    ||> List.fold parseLine
    |> fst
    |> Map.toList

let sizes =
    fs
    |> List.map (fun (path, _) ->
        fs
        |> List.filter (fun (otherPath, _) -> otherPath.StartsWith(path))
        |> List.sumBy snd)

let part1 = sizes |> List.filter (fun x -> x <= 100000) |> List.sum
printfn $"Question 1: {part1}"

let freeSpace = 70000000 - List.max sizes
let dirSize = sizes |> List.filter (fun x -> x > (30000000 - freeSpace)) |> List.min
printfn $"Question 2: {dirSize}"
