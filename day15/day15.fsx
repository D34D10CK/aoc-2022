open System.IO
open System.Text.RegularExpressions

let parseLine line =
    let matches = Regex(@"(\d+)").Matches(line)

    let coords =
        [ for m in matches do
              int m.Groups[1].Value ]

    (coords[0], coords[1]), (coords[2], coords[3])

let computeDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let circleLineIntersection (x, y) radius line =
    if abs (line - y) > radius then
        []
    elif abs (line - y) = radius then
        [ (x, line) ]
    else
        let left = abs (line - y) - radius + x
        let right = - abs(line - y) + radius + x
        printfn $"{left} {right}"

        [ for i in left..right do
              (i, line) ]

let lines = File.ReadAllLines("input.txt")
let points = lines |> Array.map parseLine
let distances = points |> Array.map ((<||) computeDistance)
let sensors = points |> Array.map fst

let line = 9

let intersections =
    (sensors, distances)
    ||> Array.zip
    |> Array.map (fun (p, d) -> circleLineIntersection p d line)
    |> List.concat
    |> set
    |> Set.count

let beaconsOnLine =
    points
    |> Array.map snd
    |> Array.filter (fun (_, y) -> y = line)
    |> set
    |> Set.count

printfn $"{intersections - beaconsOnLine}"
