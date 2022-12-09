open System.IO

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

let rec fn ropes moveTo =
    printfn $"{moveTo}"
    match ropes with
    | x :: [] -> [ move x moveTo ]
    | x :: xs ->
        let head = move x moveTo
        let rest = fn xs head.Tail
        head :: rest
    | [] -> ropes

let parseLine trail ropes (line: string) =
    let direction = line[0]
    let amount = int line[2..]

    let moves =
        match direction with
        | Up ->
            (ropes, [ 1..amount ])
            ||> List.scan (fun rs _ -> fn rs (fst rs.Head.Head, snd rs.Head.Head + 1.0))
        | Down ->
            (ropes, [ 1..amount ])
            ||> List.scan (fun rs _ -> fn rs (fst rs.Head.Head, snd rs.Head.Head - 1.0))
        | Left ->
            (ropes, [ 1..amount ])
            ||> List.scan (fun rs _ -> fn rs (fst rs.Head.Head - 1.0, snd rs.Head.Head))
        | Right ->
            (ropes, [ 1..amount ])
            ||> List.scan (fun rs _ -> fn rs (fst rs.Head.Head + 1.0, snd rs.Head.Head))
    printfn $"{moves}"
    moves |> List.map (fun rs -> (List.last rs).Tail) |> List.append trail, List.last moves

let rec parseLines trail ropes lines =
    match lines with
    | line :: rest ->
        let newTrail, newRopes = parseLine trail ropes line
        newTrail @ parseLines newTrail newRopes rest
    | [] -> trail

let lines = File.ReadAllLines("input.txt") |> Array.toList

let visitedIdx =
    parseLines [] [ { Head = (0.0, 0.0); Tail = (0.0, 0.0) } ] lines |> set

printfn $"Question 1: {visitedIdx.Count}"

let visitedIdx2 =
    parseLines
        []
        [ for _ in 1..10 do
              { Head = (0.0, 0.0); Tail = (0.0, 0.0) } ]
        lines
    |> set

printfn $"Question 2: {visitedIdx2.Count}"
