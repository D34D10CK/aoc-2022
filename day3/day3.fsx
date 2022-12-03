open System.IO


let itemPriority item =
    if System.Char.IsUpper(item) then (-) (int item) 38
    else (-) (int item) 96


let commonItem (line:string) =
    let firstCompartment = line[..line.Length / 2-1] |> Seq.toList |> set
    let secondCompartment = line[line.Length /2..] |> Seq.toList |> set

    let intersetion = Set.intersect firstCompartment secondCompartment |> Set.toList
    intersetion[0]


let fn line=
    line |> commonItem |> itemPriority



let lines = File.ReadAllLines("input.txt")
let priorities = lines |> Array.map fn |> Array.sum

printfn $"Question 1: {priorities}"
