open System.IO

let getRow (code:list<char>) =
  List.fold (fun n c -> 
    match c with
    | 'B' -> (n <<< 1 ||| 1)
    | 'F' -> (n <<< 1)
    | _ -> failwithf "Illigal character %c" c
  ) 0 code

let getCol (code:list<char>) =
  List.fold (fun n c -> 
    match c with
    | 'R' -> (n <<< 1 ||| 1)
    | 'L' -> (n <<< 1)
    | _ -> failwithf "Illigal character %c" c
  ) 0 code

let solveA (seq:list<list<char>*list<char>>) =
  List.map (fun (row, col) -> (getRow row) * 8 + (getCol col)) seq 
  |> List.max
    
let solveB (seq:list<list<char>*list<char>>) :int =
  let occupied = List.map (fun (row, col) -> (getRow row) * 8 + (getCol col)) seq 
  let freeSeats = List.except occupied [0..1032]
  List.find (fun x -> List.exists (fun y -> y = x + 1) occupied && List.exists (fun y -> y = x - 1) occupied) freeSeats

let seq =
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.map (fun (x:string) -> (x.Substring(0,7) |> Seq.toList, x.Substring(7,3) |> Seq.toList))
  |> Seq.toList

printfn "%d" (solveA seq)
printfn "%d" (solveB seq)