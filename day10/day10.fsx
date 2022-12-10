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

let lines = File.ReadAllLines("input.txt")
let stack = ([ 1 ], lines) ||> Array.fold parseLine

let sum1 =
    stack[19] * 20
    + stack[59] * 60
    + stack[99] * 100
    + stack[139] * 140
    + stack[179] * 180
    + stack[219] * 220

printfn $"Question 1: {sum1}"
