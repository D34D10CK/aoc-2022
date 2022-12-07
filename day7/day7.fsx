open System.IO
open System.Text.RegularExpressions

type Directory =
    { Parent: Directory Option
      mutable Children: Directory list
      mutable Size: int
      Name: string }

let rec flatten dir =
    match dir.Children with
    | [] -> [ dir ]
    | x -> dir :: (x |> List.map flatten |> List.concat)

let rec sizeOf dir =
    match dir.Children with
    | [] -> dir.Size
    | x ->
        List.sum
            [ for c in x do
                  sizeOf c ]
        + dir.Size

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let parseLine pwd line =
    match line with
    | ParseRegex @"^\$ cd (\w+)$" [ dir ] ->
        let newDir =
            { Parent = Some(pwd)
              Children = []
              Size = 0
              Name = dir }

        pwd.Children <- newDir :: pwd.Children
        newDir
    | "$ cd .." -> pwd.Parent.Value
    | ParseRegex @"^(\d+).*" [ size ] ->
        pwd.Size <- pwd.Size + int size
        pwd
    | _ -> pwd

let lines = File.ReadAllLines("input.txt") |> Array.toList

let rootDir =
    { Parent = None
      Children = []
      Size = 0
      Name = "/" }

(rootDir, lines.Tail) ||> List.fold parseLine
let flat = flatten rootDir
let sizes = flat |> List.map sizeOf

let part1 = sizes |> List.filter (fun x -> x <= 100000) |> List.sum
printfn $"Question 1: {part1}"

let freeSpace = 70000000 - List.max sizes
let dirSize = sizes |> List.filter (fun x -> x > (30000000 - freeSpace)) |> List.min
printfn $"Question 2: {dirSize}"
