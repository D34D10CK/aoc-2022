open System.IO

type RopeSegment =
    { Head: float * float
      Tail: float * float }

let (|Up|Down|Left|Right|) input =
    match input with
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right

let distance (x1: float, y1: float) (x2: float, y2: float) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

let moveSegment rope moveTo =
    let dist = distance rope.Tail moveTo
    let x1, y1 = moveTo
    let x2, y2 = rope.Tail

    let tail =
        if dist = 2.0 then
            ((x1 - x2) / dist + x2, (y1 - y2) / dist + y2)
        elif dist > 2 then
            let pairs = List.allPairs [ x2 - 1.0; x2 + 1.0 ] [ y2 - 1.0; y2 + 1.0 ]
            pairs |> List.minBy (fun p -> distance p moveTo)
        else
            rope.Tail

    { Head = moveTo; Tail = tail }

let rec moveRope rope moveTo =
    match rope with
    | x :: xs ->
        let head = moveSegment x moveTo
        let rest = moveRope xs head.Tail
        head :: rest
    | [] -> rope

let parseLine trail rope (line: string) =
    let direction = line[0]
    let amount = int line[2..]

    let moves =
        match direction with
        | Up ->
            (rope, [ 1..amount ])
            ||> List.scan (fun r _ -> moveRope r (fst r.Head.Head, snd r.Head.Head + 1.0))
        | Down ->
            (rope, [ 1..amount ])
            ||> List.scan (fun r _ -> moveRope r (fst r.Head.Head, snd r.Head.Head - 1.0))
        | Left ->
            (rope, [ 1..amount ])
            ||> List.scan (fun r _ -> moveRope r (fst r.Head.Head - 1.0, snd r.Head.Head))
        | Right ->
            (rope, [ 1..amount ])
            ||> List.scan (fun r _ -> moveRope r (fst r.Head.Head + 1.0, snd r.Head.Head))

    moves |> List.map (fun r -> (List.last r).Tail) |> List.append trail, List.last moves


let rec parseLines trail rope lines =
    match lines with
    | line :: rest ->
        let newTrail, newRope = parseLine trail rope line
        newTrail @ parseLines newTrail newRope rest
    | [] -> trail

let lines = File.ReadAllLines("input.txt") |> Array.toList

let visitedIdx =
    parseLines [] [ { Head = (0.0, 0.0); Tail = (0.0, 0.0) } ] lines |> set

printfn $"Question 1: {visitedIdx.Count}"

let visitedIdx2 =
    parseLines
        []
        [ for _ in 1..9 do
              { Head = (0.0, 0.0); Tail = (0.0, 0.0) } ]
        lines
    |> set

printfn $"Question 2: {visitedIdx2.Count}"
