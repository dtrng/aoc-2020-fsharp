let rec computeStatement (line:list<string>) :int64=
  let lp = List.tryFindIndexBack (fun x -> x = "(") line
  match lp with
  | Some lpi -> 
      let indexed = List.mapi (fun i c -> (i,c)) line 
      indexed |> List.filter (fun (i,_) -> i > lpi) 
      |> List.find (fun (_,c) -> c = ")")
      |> (fun (rpi,_) -> 
          (List.takeWhile (fun (i,_) -> i < lpi) indexed |> List.map (fun (i,c) -> c))
          @ (List.filter (fun (i,_) -> lpi < i && i < rpi) indexed |> List.map (fun (i,c) -> c) |> computeStatement |> (fun tmpRes -> [tmpRes.ToString()])) 
          @ ((List.filter (fun (i,_) -> i > rpi) indexed) |> List.map (fun (i,c) -> c))
        )
      |> computeStatement
  | None -> 
    line |> List.fold (fun (res, (op)) c -> 
      match c with
      | "+" -> (res, (+))
      | "*" -> (res, (*))
      | _ -> (op res (c |> int64), op)
    ) (0L, (+)) |> (fun (result,_) -> result)

let solveA (input:list<list<string>>) =
  List.sumBy computeStatement input

let rec computeStatementPlusPrio (line:list<string>) :int64=
  let lp = List.tryFindIndexBack (fun x -> x = "(") line
  match lp with
  | Some lpi -> 
      let indexed = List.mapi (fun i c -> (i,c)) line 
      indexed |> List.filter (fun (i,_) -> i > lpi) 
      |> List.find (fun (_,c) -> c = ")")
      |> (fun (rpi,_) -> 
          (List.takeWhile (fun (i,_) -> i < lpi) indexed |> List.map (fun (i,c) -> c))
          @ (List.filter (fun (i,_) -> lpi < i && i < rpi) indexed |> List.map (fun (i,c) -> c) |> computeStatementPlusPrio |> (fun tmpRes -> [tmpRes.ToString()])) 
          @ ((List.filter (fun (i,_) -> i > rpi) indexed) |> List.map (fun (i,c) -> c))
        )
      |> computeStatementPlusPrio
  | None -> 
    line |> List.fold (fun (l:list<string>, op) c -> 
      match c with
      | "+" -> (c::l, Some (+))
      | "*" -> (c::l, None)
      | _ -> 
        match op with
        | None -> (c::l, None)
        | Some o -> l |> function | _::n1::rest -> ((((n1 |> int64) + (c |> int64))|> string)::rest, None) | _ -> failwith "no previous number to add"
    ) (List.empty<string>, None) 
    |> (fun (l,_) -> l)
    |> List.fold (fun (res, (op)) c -> 
     match c with
     | "*" -> (res, (*))
     | _ -> (op res (c |> int64), op)
    ) (0L, (+)) |> (fun (result,_) -> result)

let solveB (input:list<list<string>>) =
  List.sumBy computeStatementPlusPrio input

let input =
    stdin.ReadToEnd() 
    |> (fun x -> x.Trim().Split("\n"))
    |> Seq.map (fun x -> x.Replace(" ", "") |> Seq.toList |> List.map (fun c -> c |> string))
    |> Seq.toList

printfn "%d" (solveA input)
printfn "%d" (solveB input)