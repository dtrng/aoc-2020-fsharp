open System.IO

let solveA (seq:list<int64>) =
  List.windowed (25+1) seq
  |> List.find (fun x -> 
    match List.rev x with
    | head::tail -> (head,tail)
    | [] -> failwithf "no element in list %A" x
    |> (fun (head,tail) -> 
      List.allPairs tail tail
      |> List.map (fun (a,b) -> a+b) 
      |> List.contains head
      |> not
    )
  )
  |> List.last

let solveB (input:list<int64>) =
  let invalidNumber = solveA input
  Seq.initInfinite (fun x -> x + 2)
  |> Seq.map (fun i ->
    List.windowed i input 
    |> List.tryFind (fun w -> List.sum w = invalidNumber)
  )
  |> Seq.choose id
  |> Seq.head
  |> (fun range -> List.min range + List.max range)

let seq =
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.map (fun x -> x |> int64)
  |> Seq.toList

printfn "%d" (solveA seq)
printfn "%d" (solveB seq)