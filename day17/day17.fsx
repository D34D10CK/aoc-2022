#r "nuget: FSharpx.Collections"

open System.IO
open FSharpx.Collections

let pieceList =
    let square = set [ (2, 0); (2, 1); (3, 0); (3, 1) ]
    let i = set [ (2, 0); (2, 1); (2, 2); (2, 3) ]
    let l = set [ (2, 0); (3, 0); (4, 0); (4, 1); (4, 2) ]
    let cross = set [ (3, 0); (2, 1); (3, 1); (4, 1); (3, 2) ]
    let horizontal = set [ (2, 0); (3, 0); (4, 0); (5, 0) ]

    LazyList.ofList [ horizontal; cross; l; i; square ]
    |> LazyList.repeat
    |> LazyList.concat

let parseInput text =
    LazyList.ofSeq text |> LazyList.repeat |> LazyList.concat

let moveLeft (grid: Set<int * int>) piece =
    let canGoLeft =
        piece |> Set.forall (fun (x, y) -> x < 6 && not <| grid.Contains((x + 1, y)))

    if canGoLeft then
        piece |> Set.map (fun (x, y) -> (x + 1, y))
    else
        piece

let moveRight (grid: Set<int * int>) piece =
    let canGoRight =
        piece |> Set.forall (fun (x, y) -> x > 0 && not <| grid.Contains((x - 1, y)))

    if canGoRight then
        piece |> Set.map (fun (x, y) -> (x - 1, y))
    else
        piece

let moveDirection grid dir piece =
    match dir with
    | '>' -> moveLeft grid piece
    | '<' -> moveRight grid piece

let initializePiece grid piece =
    let height = (0, grid) ||> Set.fold (fun acc (_, y) -> max acc y)
    piece |> Set.map (fun (x, y) -> (x, y + height + 4))

let hasRepeated grid =
    let height = (0, grid) ||> Set.fold (fun acc (_, y) -> max acc y)

    if height % 2 = 0 && height > 0 then
        grid
        |> Set.forall (fun (x, y) ->
            if y <= height / 2 && y > 0 then
                grid.Contains(x, y + height / 2)
            elif y > height / 2 then
                grid.Contains(x, y - height / 2)
            else
                true)
    // let bottom = grid |> Set.filter (fun (_, y) -> y <= height / 2 && y > 0)
    // let top = grid |> Set.filter (fun (_, y) -> y > height / 2)
    // top |> Set.map (fun (x, y) -> (x, y - height / 2)) = bottom
    else
        false

let rec playGame (grid: Set<int * int>) (pieces: LazyList<Set<int * int>>) (moves: LazyList<char>) iterations =
    if iterations = 0 || hasRepeated grid then
        grid
    else
        let pushedPiece = moveDirection grid moves.Head pieces.Head

        let canMoveDown =
            pushedPiece |> Set.forall (fun (x, y) -> not <| grid.Contains((x, y - 1)))

        if canMoveDown then
            let movedPiece = pushedPiece |> Set.map (fun (x, y) -> (x, y - 1))
            let newPieces = LazyList.cons movedPiece pieces.Tail

            playGame grid newPieces moves.Tail iterations
        else
            let newGrid = grid + pushedPiece
            let newPiece = pieces.Tail.Head |> initializePiece newGrid
            let newPieces = LazyList.cons newPiece pieces.Tail.Tail
            playGame newGrid newPieces moves.Tail (iterations - 1)

let text = File.ReadAllText("input.txt").Trim()
let moveList = parseInput text
let grid = set [ (0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0) ]
let pieces = LazyList.cons (pieceList.Head |> initializePiece grid) pieceList.Tail

let result = playGame grid pieces moveList 2022
let height = (0, result) ||> Set.fold (fun acc (_, y) -> max acc y)
printfn $"Question 1: {height}"


let result2 = playGame grid pieces moveList 100000000
let height2 = (0, result2) ||> Set.fold (fun acc (_, y) -> max acc y)
printfn $"Question 2: {height2}"

// let a = hasRepeated (set [ (0, 0); (1, 0); (0, 1); (1, 1); (0, 2); (1, 3) ])
// printfn $"{a}"
