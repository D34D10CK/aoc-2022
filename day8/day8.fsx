open System.IO

let isVisible i j (grid: char[,]) =
    match i, j with
    | (0, _) -> true
    | (_, 0) -> true
    | (x, _) when x = grid.GetLength 1 - 1 -> true
    | (_, x) when x = grid.GetLength 0 - 1 -> true
    | (x, y) ->
        grid[x, y] > Array.max (grid[*, y][.. x - 1])
        || grid[x, y] > Array.max (grid[*, y][x + 1 ..])
        || grid[x, y] > Array.max (grid[x, *][.. y - 1])
        || grid[x, y] > Array.max (grid[x, *][y + 1 ..])

let lines = File.ReadAllLines("input.txt")
let grid = lines |> Array.map (fun x -> x |> Seq.toArray) |> array2D

let visible =
    List.allPairs [ 0 .. grid.GetLength 1 - 1 ] [ 0 .. grid.GetLength 0 - 1 ]
    |> List.map (fun (i, j) -> isVisible i j grid)
    |> List.filter id
    |> List.length

printfn $"Question 1: {visible}"
