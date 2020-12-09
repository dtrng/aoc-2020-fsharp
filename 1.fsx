open System.IO

let solveA (seq:seq<int>) =
  let (n1, n2) = 
    Seq.allPairs seq seq
    |> Seq.find (fun (x,y) -> x + y = 2020)
  n1 * n2

let solveB (seq:seq<int>) =
  let (n1, (n2, n3)) = 
    Seq.allPairs seq seq
    |> Seq.allPairs seq
    |> Seq.find (fun (x,(y,z)) -> x + y + z = 2020)
  n1 * n2 * n3

let seq = 
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.map (fun x -> x |> int)
  |> Seq.toList

printfn "%d" (solveA seq)
printfn "%d" (solveB seq)
