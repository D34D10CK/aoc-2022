open System.IO

let isVisible i j (grid: char[,]) =
    match i, j with
    | (0, _)
    | (_, 0) -> true
    | (x, _) when x = grid.GetLength 1 - 1 -> true
    | (_, x) when x = grid.GetLength 0 - 1 -> true
    | (x, y) ->
        grid[x, y] > Array.max (grid[*, y][.. x - 1])
        || grid[x, y] > Array.max (grid[*, y][x + 1 ..])
        || grid[x, y] > Array.max (grid[x, *][.. y - 1])
        || grid[x, y] > Array.max (grid[x, *][y + 1 ..])

let viewingDistance direction height =
    let idx = direction |> Array.tryFindIndex (fun x -> x >= height)

    match idx with
    | Some(x) -> x + 1
    | None -> direction.Length

let computeScenicScore i j (grid: char[,]) =
    match i, j with
    | (0, _)
    | (_, 0) -> 0
    | (x, _) when x = grid.GetLength 1 - 1 -> 0
    | (_, x) when x = grid.GetLength 0 - 1 -> 0
    | (x, y) ->
        (grid[*, y][.. x - 1] |> Array.rev |> viewingDistance <| grid[x, y])
        * (grid[*, y][x + 1 ..] |> viewingDistance <| grid[x, y])
        * (grid[x, *][.. y - 1] |> Array.rev |> viewingDistance <| grid[x, y])
        * (grid[x, *][y + 1 ..] |> viewingDistance <| grid[x, y])

let lines = File.ReadAllLines("input.txt")
let grid = lines |> Array.map (fun x -> x |> Seq.toArray) |> array2D


let indexes =
    List.allPairs [ 0 .. grid.GetLength 1 - 1 ] [ 0 .. grid.GetLength 0 - 1 ]

let visible =
    indexes
    |> List.map (fun (i, j) -> isVisible i j grid)
    |> List.filter id
    |> List.length

printfn $"Question 1: {visible}"

let scenicScore =
    indexes |> List.map (fun (i, j: int) -> computeScenicScore i j grid) |> List.max

printfn $"Question 2: {scenicScore}"
