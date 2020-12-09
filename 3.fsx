open System.IO

let rec travel (map:list<list<char>>) ((dh,dv):(int*int)) ((h,v):int*int) (trees:int) =
  if v >= List.length map then trees
  else 
    let tree = 
      match map |> List.item v |> List.item h with
      | '#' -> 1
      | '.' -> 0
      | _ -> failwith "unexpected item"
    travel map (dh,dv) ((h+dh)%(List.length (List.head map)), v+dv) trees + tree

let solveA (map:list<list<char>>) =
  travel map (3,1) (0,0) 0

let solveB (map:list<list<char>>) =
  [(1,1);(3,1);(5,1);(7,1);(2,1)] 
  |> List.sumBy (fun x -> (travel map x (0,0) 0))

let map =
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.map Seq.toList
  |> Seq.toList

printfn "%d" (solveA map)
printfn "%d" (solveB map)