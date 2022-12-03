open System.IO

let itemPriority item =
    if System.Char.IsUpper(item) then
        (-) (int item) 38
    else
        (-) (int item) 96

let splitRucksack (line: string) =
    let firstCompartment = line[.. line.Length / 2 - 1]
    let secondCompartment = line[line.Length / 2 ..]
    [| firstCompartment; secondCompartment |]

let commonItem (bags: string[]) =
    let commonItems =
        bags
        |> Array.map (fun x -> x |> Seq.toList |> set)
        |> Array.reduce Set.intersect
        |> Set.toList

    commonItems[0]

let computeRucksackPriorities line =
    line |> splitRucksack |> commonItem |> itemPriority

let computeElfGroupsPriorities group = group |> commonItem |> itemPriority

let lines = File.ReadAllLines("input.txt")
let rucksackPriority = lines |> Array.map computeRucksackPriorities |> Array.sum
printfn $"Question 1: {rucksackPriority}"

let elfGroups =
    [ for i in 0..3 .. lines.Length - 1 do
          yield lines[i .. i + 2] ]

let badgePriority = elfGroups |> List.map computeElfGroupsPriorities |> List.sum
printfn $"Question 2: {badgePriority}"
