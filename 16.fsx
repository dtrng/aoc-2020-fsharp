open System.Text.RegularExpressions

let validNumber t rules =
  List.tryFind (fun (_,f1,t1,f2,t2) -> (f1 <= t && t <= t1) || (f2 <= t && t <= t2)) rules
  |> function
    | Some _ -> true
    | None -> false

let solveA (rules:list<string*int*int*int*int>) (myTicket:list<int>) (otherTickets:list<list<int>>) =
  otherTickets 
  |> List.sumBy (fun ticket -> List.sumBy (fun t -> if (validNumber t rules) then 0 else t) ticket)

let solveB (rules:list<string*int*int*int*int>) (myTicket:list<int>) (otherTickets:list<list<int>>) =
  let validTickets = 
    otherTickets |> List.filter (fun ticket -> (List.sumBy (fun t -> if (validNumber t rules) then 0 else 1) ticket) = 0)
  let rulesWithValidCols =
    rules 
    |> List.map (fun (name,f1,t1,f2,t2) -> 
      let invalidColumns = 
        validTickets 
        |> List.map (fun ticket ->
            List.mapi (fun i col -> (i, col)) ticket
            |> List.filter (fun (_,t) -> ((f1 <= t && t <= t1) || (f2 <= t && t <= t2)) |> not)
            |> List.map (fun (i,_) -> i)
            |> Set.ofList
        )
        |> Set.unionMany
      (name, Seq.except invalidColumns (Seq.ofList [0..19]) |> Seq.toList))
  let (_,res) = 
    List.fold (fun (rules,res) _ ->
      let (foundRuleName,foundRuleCols) = List.find (fun (_,x) -> 1 = List.length x) rules
      (
        List.map (fun (name,validCols) -> (name, List.except foundRuleCols validCols)) rules, 
        (foundRuleName,foundRuleCols)::res
      )
    ) (rulesWithValidCols, List.empty) [0..19]
  res 
  |> List.filter (fun (name, _) -> name.StartsWith "departure")
  |> List.map (fun (_,y) -> List.item (List.head y) myTicket |> int64)
  |> List.fold (*) 1L

let (rules, myTicket, otherTickets) =
  let rulePattern = "([\w\s]+):\s(\d+)-(\d+)\sor\s(\d+)-(\d+)"
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n\n"))
  |> Seq.toList
  |> function 
    | zones::myTicket::otherTickets::_-> (zones,myTicket,otherTickets)
    | _ -> failwithf "invalid input"
  |> (fun (zones,myTicket,otherTickets) -> 
  (
    zones.Split('\n') |> Seq.map (
      fun x -> 
        Regex.Match(x, rulePattern).Groups 
        |> Seq.toList 
        |> function 
          |_::section::from1::to1::from2::to2::_ -> 
          (
            section.Value, 
            from1.Value |> int, 
            to1.Value |> int, 
            from2.Value |> int, 
            to2.Value |> int) 
          | _ -> failwith "invalid regex group")
    |> Seq.toList,
    myTicket.Split('\n') |> Seq.last |> (fun x -> x.Split(',')) |> Seq.map (fun x -> x |> int) |> Seq.toList, 
    otherTickets.Split('\n') |> Seq.tail |> Seq.map (fun t -> t.Split(',') |> Seq.map (fun x -> x |> int) |> Seq.toList)|> Seq.toList)
  )

printfn "%d" (solveA rules myTicket otherTickets)
printfn "%d" (solveB rules myTicket otherTickets)
