open System.IO

let parseLine stack (line: string) =
    let op = line[..3]
    let top = stack |> List.last

    let cycles =
        match op with
        | "noop" -> [ top ]
        | "addx" ->
            let x = int line[5 .. line.Length - 1]
            [ top; top + x ]

    stack @ cycles

let drawPixel (stack: int list) cycle =
    let spriteLeft = stack[cycle] - 1
    let spriteRight = stack[cycle] + 1
    let position = cycle % 40

    if position >= spriteLeft && position <= spriteRight then
        '#'
    else
        '.'

let lines = File.ReadAllLines("input.txt")
let stack = ([ 1 ], lines) ||> Array.fold parseLine

let part1 =
    [ for i in [ 20..40..220 ] do
          stack[i - 1] * i ]
    |> List.sum

printfn $"Question 1: {part1}"

let crt = [| 0..239 |] |> Array.map (fun x -> drawPixel stack x) |> System.String

let crt2d =
    [ for i in [ 0..40..200 ] do
          crt[i .. i + 39] ]
    |> String.concat "\n"

printfn "Question 2:"
printfn $"{crt2d}"
