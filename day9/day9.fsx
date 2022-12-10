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

let printState (ropes: Rope list) =
    let grid =
        [ for _ in 1..35 do
              [ for _ in 1..35 do
                    '.' ] ]
        |> array2D

    grid[15, 15] <- 'S'

    grid[int (fst ropes[0].Head) + 15, int (snd ropes[0].Head) + 15] <- 'H'

    for i = 0 to ropes.Length - 2 do
        grid[int (fst ropes[i].Tail) + 15, int (snd ropes[i].Tail) + 15] <- $"{i + 1}"[0]

    grid[int (fst ropes[ropes.Length - 1].Tail) + 15, int (snd ropes[ropes.Length - 1].Tail) + 15] <- 'T'


    for i = 0 to Array2D.length1 grid - 1 do
        for j = 0 to Array2D.length2 grid - 1 do
            printf $"{grid[i, j]}"

        printfn ""

let distance (x1: float, y1: float) (x2: float, y2: float) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

let move rope moveTo =
    let dist = distance rope.Tail moveTo
    // printf $"head: {rope.Head} tail: {rope.Tail} moveTo: {moveTo} dist: {dist}"

    // let res =
    //     if dist < 2 && dist > 1 then
    //         { Head = moveTo; Tail = rope.Tail }
    //     elif rope.Head = moveTo then
    //         rope
    //     elif dist = 2.0 then
    //         let x1, y1 = moveTo
    //         let x2, y2 = rope.Tail

    //         { Head = moveTo
    //           Tail = ((x1 - x2) / dist + x2, (y1 - y2) / dist + y2) }
    //     else
    //         { Head = moveTo; Tail = rope.Head }
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


    let res = { Head = moveTo; Tail = tail }
    // printfn $"-> head: {res.Head} tail: {res.Tail}"
    res


let rec fn ropes moveTo =
    // printfn $"{moveTo}"
    match ropes with
    // | x :: [] -> [ move x moveTo ]
    | x :: xs ->
        let head = move x moveTo
        let rest = fn xs head.Tail
        head :: rest
    | [] -> ropes

let parseLine trail ropes (line: string) =
    // trail |> List.iter (fun x -> printfn $"{x}")
    // ropes |> List.iter (fun x -> printfn $"{x}")
    // printfn "------------"

    // printState ropes
    // printfn "----------------"
    let direction = line[0]
    let amount = int line[2..]
    // printfn $"{line}"

    let moves =
        match direction with
        | Up ->
            (ropes, [ 1..amount ])
            ||> List.scan (fun rs _ ->
                // printState rs
                fn rs (fst rs.Head.Head, snd rs.Head.Head + 1.0))
        | Down ->
            (ropes, [ 1..amount ])
            ||> List.scan (fun rs _ ->
                // printState rs
                fn rs (fst rs.Head.Head, snd rs.Head.Head - 1.0))
        | Left ->
            (ropes, [ 1..amount ])
            ||> List.scan (fun rs _ ->
                // printState rs
                fn rs (fst rs.Head.Head - 1.0, snd rs.Head.Head))
        | Right ->
            (ropes, [ 1..amount ])
            ||> List.scan (fun rs _ ->
                // printState rs
                fn rs (fst rs.Head.Head + 1.0, snd rs.Head.Head))
    // moves |> List.iter (fun x -> printfn $"{x}")

    let res =
        moves |> List.map (fun rs -> (List.last rs).Tail) |> List.append trail, List.last moves
    // printState (snd res)
    // printfn "----------------"
    res

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
        [ for _ in 1..9 do
              { Head = (0.0, 0.0); Tail = (0.0, 0.0) } ]
        lines
    |> set
// visitedIdx2 |> List.iter (fun x -> printfn $"{x}")
printfn $"Question 2: {visitedIdx2.Count}"

// let grid =
//     [ for _ in 1..35 do
//           [ for _ in 1..35 do
//                 '.' ] ]
//     |> array2D

// for i = 0 to visitedIdx2.Length - 1 do
//     grid[int (fst visitedIdx2[i]) + 15, int (snd visitedIdx2[i]) + 15] <- '#'

// for i = 0 to Array2D.length1 grid - 1 do
//     for j = 0 to Array2D.length2 grid - 1 do
//         printf $"{grid[i, j]}"

//     printfn ""
