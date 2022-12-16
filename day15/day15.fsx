open System.IO
open System.Text.RegularExpressions

let parseLine line =
    let matches = Regex(@"(-?\d+)").Matches(line)

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

        [ for i in left..right do
              (i, line) ]

let makeCircle (x, y) radius =
    [ for i in 0..radius do
          [ (x + radius - i, y + i)
            (x + radius - i, y - i)
            (x - radius + i, y - i)
            (x - radius + i, y + i) ] ]
    |> List.concat
    |> set

let lines = File.ReadAllLines("input.txt")
let points = lines |> Array.map parseLine
let distances = points |> Array.map ((<||) computeDistance)
let sensors = points |> Array.map fst

let line = 2000000

let sensorsDistances = (sensors, distances) ||> Array.zip

let intersections =
    sensorsDistances
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

printfn $"Question 1: {intersections - beaconsOnLine}"

let x, y =
    sensorsDistances
    |> Array.Parallel.map (fun (c, r) ->
        makeCircle c (r + 1)
        |> Set.filter (fun (x, y) ->
            x <= 4000000
            && y <= 4000000
            && x >= 0
            && y >= 0
            && Array.forall (fun (c2, r) -> computeDistance (x, y) c2 > r) sensorsDistances))
    |> Array.filter (fun c -> not c.IsEmpty)
    |> Set.intersectMany
    |> Set.toList
    |> List.exactlyOne

printfn $"Question 2: {int64 (x) * 4000000L + int64 (y)}"
