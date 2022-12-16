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

let getVertices (x, y) r =
    let p1 = x + r, y
    let p2 = x - r, y
    let p3 = x, y + r
    let p4 = x, y - r
    [ p1; p2; p3; p4 ]

let circleIntersection (x1, y1) r1 (x2, y2) r2 =
    let d = computeDistance (x1, y1) (x2, y2)

    if x1 = x2 && y1 = y2 then
        []
    elif r1 + r2 <> d then
        []
    else
        let v1 =
            (getVertices (x1, y1) r1)
            |> List.filter (fun (x, y) -> computeDistance (x, y) (x2, y2) = r2)

        let v2 =
            (getVertices (x2, y2) r2)
            |> List.filter (fun (x, y) -> computeDistance (x, y) (x1, y1) = r1)

        v1 @ v2

let singleIntersection2 c1 r1 c2 r2 c3 r3 =
    let v1 =
        (getVertices c1 r1)
        |> List.filter (fun (x, y) -> computeDistance (x, y) c2 = r2 && computeDistance (x, y) c3 = r3)

    let v2 =
        (getVertices c2 r2)
        |> List.filter (fun (x, y) -> computeDistance (x, y) c1 = r1 && computeDistance (x, y) c3 = r3)

    let v3 =
        (getVertices c3 r3)
        |> List.filter (fun (x, y) -> computeDistance (x, y) c1 = r1 && computeDistance (x, y) c2 = r2)

    v1 @ v2 @ v3

let lines = File.ReadAllLines("input.txt")
let points = lines |> Array.map parseLine
let distances = points |> Array.map ((<||) computeDistance)
let sensors = points |> Array.map fst

let line = 2000000

let sensorsDistances = (sensors, distances) ||> Array.zip

// let intersections =
//     sensorsDistances
//     |> Array.map (fun (p, d) -> circleLineIntersection p d line)
//     |> List.concat
//     |> set
//     |> Set.count

// let beaconsOnLine =
//     points
//     |> Array.map snd
//     |> Array.filter (fun (_, y) -> y = line)
//     |> set
//     |> Set.count

// printfn $"Question 1: {intersections - beaconsOnLine}"

let pairs = (sensorsDistances, sensorsDistances) ||> Array.allPairs

let a =
    pairs
    |> Array.map (fun ((c1, r1), (c2, r2)) -> circleIntersection c1 (r1 + 1) c2 (r2 + 1))
    |> Array.filter (fun l -> l.Length > 0)
    |> Array.map set
    |> Array.reduce (+)
    |> Set.filter (fun (x, y) -> x <= 4000000 && y <= 4000000 && x >= 0 && y >= 0)
    |> Set.filter (fun c1 ->
        sensorsDistances
        |> Array.forall (fun (c2, r) ->
            // printfn $"{r}"
            // printfn $"{computeDistance c1 c2}"
            computeDistance c1 c2 > r))

for i in a do
    printfn $"{i}"
// for (c1, r) in sensorsDistances do
//     if computeDistance c1 i > r then
//         printfn $"{c1} {r}"

// for j in i do
//     printfn $"{j}"
