open System
let solveA (input:list<int>) =
  let N = 2020
  let mutable numbers = List.mapi (fun i n -> (i + 1, n) ) input |> List.rev
  for x in [(List.length input + 1)..N] do
    let (lastTime, last) = List.head numbers
    let prev = List.skip 1 numbers |> List.tryFind (fun (time, number) -> number = last)
    match prev with
    | None -> numbers <- (x,0)::numbers
    | Some (t, _) -> numbers <- (x,lastTime - t)::numbers

  List.find (fun (t,_) -> t = N) numbers
  |> (fun (_,v) -> v)

let solveB (input:list<int>) =
  let N = 30000000
  let head::tail = List.mapi (fun i n -> (n, i + 1) ) input |> List.rev
  let ((res,_), _) =
    List.fold (fun (last,map) x -> 
      let (lastVal, lastTime) = last
      let prev = Map.tryFind lastVal map
      match prev with
      | None -> ((0, x), Map.change lastVal (fun _ -> Some lastTime) map)
      | Some t -> ((lastTime - t, x), Map.change lastVal (fun _ -> Some lastTime) map)
    ) (head, tail |> Map.ofList) [(List.length input + 1)..N]
  res

let input =
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split(","))
  |> Seq.map (fun x -> x |> int)
  |> Seq.toList

printfn "%d" (solveA input)
printfn "%d" (solveB input)
