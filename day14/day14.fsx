open System.IO
open System.Text.RegularExpressions

let pointsToSegment (x1, y1) (x2, y2) =
    if x1 = x2 then
        [ for i = 0 to abs (y1 - y2) do
              (x1, min y1 y2 + i) ]
    else
        [ for i = 0 to abs (x1 - x2) do
              (min x1 x2 + i, y1) ]

let coordsToSegments coords =
    let rec recurse points =
        match points with
        | _ :: [] -> points
        | x1 :: x2 :: xs -> pointsToSegment x1 x2 @ recurse (x2 :: xs)

    coords |> recurse |> set

let rec pourSand1 (rocks: Set<int * int>) sand =
    let (x, y) = sand
    let hole = rocks |> Set.map snd |> Set.maxElement

    if y = hole then
        rocks
    else if rocks.Contains(x, y + 1) then
        if rocks.Contains(x - 1, y + 1) then
            if rocks.Contains(x + 1, y + 1) then
                pourSand1 (rocks.Add(sand)) (500, 0)
            else
                pourSand1 rocks (x + 1, y + 1)
        else
            pourSand1 rocks (x - 1, y + 1)
    else
        pourSand1 rocks (x, y + 1)

let rec pourSand2 (rocks: Set<int * int>) sand floor =
    let (x, y) = sand
    let onFloor = floor = y + 1

    if rocks.Contains(x, y + 1) || onFloor then
        if rocks.Contains(x - 1, y + 1) || onFloor then
            if rocks.Contains(x + 1, y + 1) || onFloor then
                if rocks.Contains(500, 0) then
                    rocks
                else
                    pourSand2 (rocks.Add(sand)) (500, 0) floor
            else
                pourSand2 rocks (x + 1, y + 1) floor
        else
            pourSand2 rocks (x - 1, y + 1) floor
    else
        pourSand2 rocks (x, y + 1) floor


let parseLine line =
    let matches = Regex(@"(\d+),(\d+)").Matches(line)

    let points =
        [ for m in matches do
              int m.Groups[1].Value, int m.Groups[2].Value ]

    coordsToSegments points

let lines = File.ReadAllLines("input.txt")
let rocks = lines |> Array.map parseLine |> Array.reduce (+)

let startingSize = rocks.Count

let finalState = pourSand1 rocks (500, 0)
printfn $"Question 1: {finalState.Count - startingSize}"

let floor = (rocks |> Set.map snd |> Set.maxElement) + 2
let finalState2 = pourSand2 rocks (500, 0) floor
printfn $"Question 2: {finalState2.Count - startingSize}"
