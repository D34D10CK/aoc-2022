open System.IO
open System.Text.RegularExpressions

type Rope =
    { Head: float * float
      Tail: float * float }

let (|Up|Down|Left|Right|) input =
    match input with
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right

let distance (x1: float, y1: float) (x2: float, y2: float) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

let move rope moveTo =
    let dist = distance rope.Tail moveTo

    if dist < 2 then
        { Head = moveTo; Tail = rope.Tail }
    else
        { Head = moveTo; Tail = rope.Head }

let parseLine trail rope line =
    let m = Regex(@"(\w) (\d+)").Match(line)
    let direction = m.Groups[1].Value[0]
    let amount = int m.Groups[2].Value
    let x, y = rope.Head

    let moves =
        match direction with
        | Up -> (rope, [ 1..amount ]) ||> List.scan (fun r i -> move r (x, y + float i))
        | Down -> (rope, [ 1..amount ]) ||> List.scan (fun r i -> move r (x, y - float i))
        | Left -> (rope, [ 1..amount ]) ||> List.scan (fun r i -> move r (x - float i, y))
        | Right -> (rope, [ 1..amount ]) ||> List.scan (fun r i -> move r (x + float i, y))

    moves |> List.map (fun r -> r.Tail) |> List.append trail, List.last moves

let rec parseLines trail rope lines =
    match lines with
    | line :: rest ->
        let newTrail, newRope = parseLine trail rope line
        newTrail @ parseLines newTrail newRope rest
    | [] -> trail

let lines = File.ReadAllLines("input.txt") |> Array.toList
let visitedIdx = parseLines [] { Head = (0.0, 0.0); Tail = (0.0, 0.0) } lines |> set

printfn $"Question 1: {visitedIdx.Count}"
