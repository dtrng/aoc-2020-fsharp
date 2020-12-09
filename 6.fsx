open System.IO

let solveA (input:list<list<string>>) =
  List.sumBy (fun x -> 
    List.fold (fun str s -> str+s) "" x 
    |> Seq.toList 
    |> List.distinct 
    |> List.length) input

let solveB (input:list<list<string>>) =
  List.map (fun x ->
    List.filter (fun (x:string) -> x.Length > 0) x
    |> List.map ((fun (y:string) -> y.Trim().Replace("\n", "")) >> Set.ofSeq)
    |> Set.intersectMany
    ) input
  |> List.sumBy Set.count

let seq =
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim())
  |> (fun file -> file.Split "\n\n")
  |> Seq.map (fun doc -> doc.Split("\n") |> Array.toList)
  |> Seq.toList

printfn "%d" (solveA seq)
printfn "%d" (solveB seq)
