open System.IO

let pieceList =
    let square = set [ (2, 0); (2, 1); (3, 0); (3, 1) ]
    let i = set [ (2, 0); (2, 1); (2, 2); (2, 3) ]
    let l = set [ (2, 0); (3, 0); (4, 0); (4, 1); (4, 2) ]
    let cross = set [ (3, 0); (2, 1); (3, 1); (4, 1); (3, 2) ]
    let horizontal = set [ (2, 0); (3, 0); (4, 0); (5, 0) ]
    Seq.initInfinite (fun _ -> [ horizontal; cross; l; i; square ]) |> Seq.concat

let parseInput text =
    Seq.initInfinite (fun _ -> Seq.toList text) |> Seq.concat

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
    // printfn $"{dir}"

    match dir with
    | '>' -> moveLeft grid piece
    | '<' -> moveRight grid piece

let initializePiece grid piece =
    let height = (0, grid) ||> Set.fold (fun acc (_, y) -> max acc y)
    let a = piece |> Set.map (fun (x, y) -> (x, y + height + 4))
    // printfn "initialize"

    // for i in a do
    //     printfn $"{i}"

    a

let rec playGame (grid: Set<int * int>) (pieces: seq<Set<int * int>>) (moves: char seq) iterations =
    printfn $"{iterations}"
    if iterations = 0 then
        grid
    else
        let pushedPiece = moveDirection grid (Seq.head moves) (Seq.head pieces)

        let canMoveDown =
            pushedPiece |> Set.forall (fun (x, y) -> not <| grid.Contains((x, y - 1)))

        if canMoveDown then
            let movedPiece = pushedPiece |> Set.map (fun (x, y) -> (x, y - 1))

            let newPieces =
                seq {
                    yield movedPiece
                    yield! Seq.tail pieces
                }

            playGame grid newPieces (Seq.tail moves) iterations
        else
            // printfn $"{grid.Count}"
            let newGrid = grid + pushedPiece
            let newPiece = pieces |> Seq.tail |> Seq.head |> initializePiece newGrid

            let newPieces =
                seq {
                    yield newPiece
                    yield! pieces |> Seq.tail |> Seq.tail
                }


            // printfn "new piece"
            playGame newGrid newPieces (Seq.tail moves) (iterations - 1)

let text = File.ReadAllText("input.txt").Trim()
let moveList = parseInput text
let grid = set [ (0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0) ]

let pieces =
    seq {
        yield pieceList |> Seq.head |> initializePiece grid
        yield! Seq.tail pieceList
    }

let result = playGame grid pieces moveList 2022

for i in result do
    printfn $"{i}"

let height = (0, result) ||> Set.fold (fun acc (_, y) -> max acc y)
printfn $"{height}"
