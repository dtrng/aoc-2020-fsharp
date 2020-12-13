type BusLine = {Id: int64; Position: int64}
type Bus = {Id: int64; WaitingTime: int64}

let solveA arriveTime (busLines:seq<BusLine>) =
  busLines 
  |> Seq.map (fun busLine -> 
  { 
    Id = busLine.Id; 
    WaitingTime = (arriveTime / busLine.Id + 1L) * busLine.Id - arriveTime
  })
  |> Seq.sortBy (fun bus -> bus.WaitingTime)
  |> Seq.head |> (fun bus -> bus.Id * bus.WaitingTime)

let solveB (busLines:seq<BusLine>) =
  let firstLine::otherLines = busLines |> Seq.sortByDescending (fun x -> x.Id) |> Seq.toList
  let mutable multiple = firstLine.Id
  let mutable num = firstLine.Id - (firstLine.Position % firstLine.Id)

  for line in otherLines do
    let rest = (line.Id - (line.Position % line.Id)) % line.Id
    num <- 
      Seq.initInfinite (fun i -> num + multiple * (i |> int64))
      |> Seq.find (fun x -> x % line.Id = rest)
    multiple <- multiple * line.Id
  num

let (arriveTime, busLines) =
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n") |> Seq.toList)
  |> (fun lines -> 
    match lines with
    | minTime::busLines::_ -> 
      (
        minTime |> int64, 
        busLines.Split(',') 
        |> Seq.mapi (fun position id -> (position,id)) 
        |> Seq.filter (fun (_, id) -> id <> "x")
        |> Seq.map (fun (position,id) -> {Id = id |> int64; Position = position |> int64}) 
      )
    | _ -> failwithf "Unexpected input array %A" lines
  )

printfn "%d" (solveA arriveTime busLines)
printfn "%d" (solveB busLines)
