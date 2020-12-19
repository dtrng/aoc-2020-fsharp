let getNeighbors3D (cube:uint64) = 
  let ops = [(+);(-);(fun x _ -> x )]
  List.allPairs [0..2] (List.allPairs [0..2] [0..2])
  |> List.map 
    ((fun (i,(j,k)) -> (List.item i ops, List.item j ops, List.item k ops) )
    >> (fun (p,q,r) -> r (q (p cube 1UL) (1UL<<<16)) (1UL<<<32)))

let solveA (initCubes:Set<uint64>) =
  Seq.fold (fun activeCubes t -> 
    let allNeighbors =
      activeCubes 
      |> Set.fold (fun neighbors square -> (getNeighbors3D square) @ neighbors) List.empty
      |> Set.ofList
    allNeighbors
    |> Set.filter (fun box -> 
      let activeNeighbors = (getNeighbors3D box) |> Seq.except (Seq.except [box] activeCubes) |> Seq.length |> (fun x -> 27 - x)
      match (Set.contains box activeCubes) with
      | true -> activeNeighbors = 2 || activeNeighbors = 3
      | false -> activeNeighbors = 3 
    )
  ) initCubes [0..5]
  |> Set.count

let getNeighbors4D (cube:uint64) = 
  let ops = [(+);(-);(fun x _ -> x )]
  List.allPairs [0..2] (List.allPairs [0..2] (List.allPairs [0..2] [0..2]))
  |> List.map 
    ((fun (i,(j,(k,l))) -> (List.item i ops, List.item j ops, List.item k ops, List.item l ops) )
    >> (fun (p,q,r,s) -> s (r (q (p cube 1UL) (1UL<<<16)) (1UL<<<32)) (1UL<<<48)))

let solveB (initCubes:Set<uint64>) =
  Seq.fold (fun activeCubes t -> 
    let allNeighbors =
      activeCubes 
      |> Set.fold (fun neighbors square -> (getNeighbors4D square) @ neighbors) List.empty
      |> Set.ofList
    allNeighbors
    |> Set.filter (fun cube -> 
      let activeNeighbors = 
        (getNeighbors4D cube) 
        |> List.except (Seq.except [cube] activeCubes) 
        |> List.length 
        |> (fun x -> 81 - x)
      match (Set.contains cube activeCubes) with
      | true -> activeNeighbors = 2 || activeNeighbors = 3
      | false -> activeNeighbors = 3 
    )
  ) initCubes [0..5]
  |> Set.count

let input =
  let dimOrigin = pown 2 15 |> uint64
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.mapi (fun row x -> 
    Seq.toList x 
    |> Seq.mapi (fun col y -> (col |> uint64,y))
    |> Seq.filter (fun (_,y) -> y = '#')
    |> Seq.map (fun (col,_) -> (col + dimOrigin) + (((row |> uint64) + dimOrigin) <<< 16) + (dimOrigin <<< 32) + (dimOrigin <<< 48))
  )
  |> Seq.concat
  |> Set.ofSeq

printfn "%d" (solveA input)
printfn "%d" (solveB input)