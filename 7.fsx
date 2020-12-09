open System.IO
open System.Text.RegularExpressions

let bagsContainingBag (key:string) (map:Map<string,list<string*int>>) :list<string> =
  Map.filter (fun k v -> List.exists (fun (x,_) -> x = key) v) map
  |> Map.fold (fun (l:list<string>) k _ -> k::l) list.Empty

let rec wrappingBagTypes (map:Map<string,list<string*int>>) (bags) =
  match bags with
  | [] -> Set.empty
  | head::tail -> 
    let wrappingBags = bagsContainingBag head map
    Set.union (Set.ofList wrappingBags) (wrappingBagTypes map (List.append tail wrappingBags))

let solveA (map:Map<string,list<string*int>>) =
  let res = wrappingBagTypes map ["shiny gold"]
  Set.count res

let rec innerBags (map:Map<string,list<string*int>>) (box:string) =
  let boxes = Map.find box map
  List.sumBy (fun (b,ib) -> ib * (1 + innerBags map b) ) boxes

let solveB (map:Map<string,list<string*int>>) =
  innerBags map "shiny gold"

let seq = 
  let keyPattern = "([\w\s]+)\sbags\scontain"
  let valuesPattern = "\s(\d+)\s([\w\s]+)\sbag"
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.map 
    (fun x -> 
      Regex.Match(x, keyPattern).Groups |> Seq.last |> (fun x -> x.Value), 
      Regex.Matches(x, valuesPattern) |> Seq.map (fun x -> 
        match Seq.toList x.Groups with 
        |_::n::bag::_ -> (bag.Value,n.Value |> int) 
        |_->failwithf "Did not match %A" x.Groups)
    ) 
  |> Seq.fold (fun (map:Map<string,list<string*int>>) (k, v) -> Map.add k (Seq.toList v) map) Map.empty

printfn "%d" (solveA seq)
printfn "%d" (solveB seq)