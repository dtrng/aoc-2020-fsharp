let solveA (seq:list<int64>) =
  List.append seq [0L;(List.max seq) + 3L]
  |> List.sort
  |> List.pairwise
  |> List.fold (fun (ones, threes) (x,y) -> 
    match y-x with
    | 1L -> (ones + 1, threes) 
    | 2L -> (ones, threes)
    | 3L -> (ones, threes + 1) 
    | _ -> failwithf "Unexpected diff %d" (y-x)
    ) (0,0)
  |> (fun (ones,threes) -> ones*threes)

let rec findCombinations adapters joltage (lookup:Map<int64,int64>) :Map<int64,int64> =
  if joltage = List.max adapters then 
    Map.change joltage (fun _ -> Some 1L) lookup
  else 
    let compatibleAdapters = List.filter (fun x -> joltage + 1L <= x &&  x <= joltage + 3L) adapters

    let extendedLookup = 
      List.filter (fun x -> Map.containsKey x lookup |> not) compatibleAdapters
      |> List.fold (fun arrengements adapter -> findCombinations adapters adapter arrengements) lookup

    let arrengements = List.sumBy (fun x -> Map.find x extendedLookup) compatibleAdapters
    Map.change joltage (fun _ -> Some arrengements) extendedLookup

let solveB (adapters:list<int64>) =
  findCombinations (List.append adapters [0L; (List.max adapters) + 3L]) 0L Map.empty<int64,int64>
  |> Map.find 0L

let seq =
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.map (fun x -> x |> int64)
    |> Seq.toList

printfn "%d" (solveA seq)
printfn "%d" (solveB seq)
