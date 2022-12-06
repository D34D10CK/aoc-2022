open System.IO

let allDistinct (str: char[]) = str.Length = (set str).Count

let firstAllDistinctWindowIndex str windowSize =
    str |> Seq.windowed windowSize |> Seq.map allDistinct |> Seq.findIndex id

let findFirstMarkerPosition str = firstAllDistinctWindowIndex str 4 + 4

let findMessageStart str = firstAllDistinctWindowIndex str 14 + 14

let line = File.ReadAllText("input.txt")

let markerIndex = findFirstMarkerPosition line
printfn $"Question 1: {markerIndex}"

let messageStart = findMessageStart line
printfn $"Question 2: {messageStart}"
