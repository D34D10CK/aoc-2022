open System.IO

type Range = { Start: int; End: int }

let fullyContained range1 range2 =
    let s1 = set [ range1.Start .. range1.End ]
    let s2 = set [ range2.Start .. range2.End ]
    let union = Set.union s1 s2
    s1 = union || s2 = union

let overlapping range1 range2 =
    let s1 = set [ range1.Start .. range1.End ]
    let s2 = set [ range2.Start .. range2.End ]
    let intersection = Set.intersect s1 s2
    not intersection.IsEmpty

let parseRange (str: string) =
    let bounds = str.Split '-'
    let left = int bounds[0]
    let right = int bounds[1]
    { Start = left; End = right }

let parseLine (line: string) =
    let pair = line.Split ','
    let r1 = parseRange pair[0]
    let r2 = parseRange pair[1]
    r1, r2

let findOverlapping line =
    let r1, r2 = parseLine line
    overlapping r1 r2

let lines = File.ReadAllLines("input.txt")

let numFullyContainedIntervals =
    lines
    |> Array.map parseLine
    |> Array.filter (fun x -> x ||> fullyContained)
    |> Array.length

printfn $"Question 1: {numFullyContainedIntervals}"

let numOverlappingIntervals =
    lines
    |> Array.map parseLine
    |> Array.filter (fun x -> x ||> overlapping)
    |> Array.length

printfn $"Question 2: {numOverlappingIntervals}"
