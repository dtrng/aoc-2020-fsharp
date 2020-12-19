open System.Text.RegularExpressions

let rec validPatterns (map:Map<int,string>) word wp ruleId = 
  if wp = Seq.length word then 
    List.empty
  else
    let groups = Map.find ruleId map |> (fun (x:string) -> x.Split("|") |> Seq.map (fun x -> x.Trim())) |> Seq.toList
    match groups with
    | ["\"a\""] -> if (Seq.item wp word) = 'a' then [wp+1] else List.empty
    | ["\"b\""] -> if (Seq.item wp word) = 'b' then [wp+1] else List.empty
    | _ -> 
      List.fold (fun valid (group:string) -> 
        valid @ (group.Split(" ") 
        |> Seq.fold (fun vid y -> 
          match vid with
          | [] -> []
          | _ -> Seq.map (fun i -> validPatterns map word i (y|>int)) vid |> List.concat
        ) [wp])
      ) List.empty groups 

let solveA (rules:seq<int*string>) (messages:seq<string>) =
  let map = Map.ofSeq rules
  Seq.sumBy (fun m ->
    let res = validPatterns map m 0 0
    if Seq.contains (m |> Seq.length) res then 1 else 0
  ) messages

let solveB (rules:seq<int*string>) (messages:seq<string>) =
  let map = 
    Map.ofSeq rules 
    |> Map.change 8 (fun _ -> Some "42 | 42 8") 
    |> Map.change 11 (fun _ -> Some "42 31 | 42 11 31") 
  Seq.sumBy (fun m ->
    let res = validPatterns map m 0 0
    if Seq.contains (m |> Seq.length) res then 1 else 0
  ) messages

let rulePattern = "(\d+):(.*)"
let (rules, messages) =
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n\n") |> Array.toList)
  |> function
    | rules::messages::_ -> 
      (
        rules.Split("\n") 
        |> Seq.map (fun x -> 
          Regex.Match(x, rulePattern).Groups 
          |> Seq.toList 
          |> function 
            | _::num::rule::_ -> (num.Value |> int, rule.Value.Trim())
            | _ -> failwith "Invaid rule"),
        messages.Split("\n") |> Seq.map (fun x -> x.Trim())
      )
    | _ -> failwith "invalid input"

printfn "%d" (solveA rules messages)
printfn "%d" (solveB rules messages)
