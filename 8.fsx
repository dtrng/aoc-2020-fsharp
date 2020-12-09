open System.IO

let replace index item = 
  List.mapi (fun i current -> if i = index then item else current)

let rec execute (instructions:list<string*int*int>) (ptr:int) (acc:int) =
  if (ptr >= List.length instructions) then 
    (acc,0)
  else
    let (op, n, calls) = List.item ptr instructions 
    if (calls > 0) then
      (acc,1)
    else
      let newInstructions = replace ptr (op,n,calls + 1) instructions
      match op with
      | "acc" -> execute newInstructions (ptr+1) (acc+n)
      | "jmp" -> execute newInstructions (ptr+n) acc
      | "nop" -> execute newInstructions (ptr+1) acc
      | _ -> failwithf "Unexpected operation %s" op

let solveA (seq:list<string*int*int>) =
  let (acc, _) = execute seq 0 0
  acc

let solveB (seq:list<string*int*int>) =
  let (acc,_) = 
    List.mapi (fun i v -> (i,v)) seq
    |> List.filter (fun (_,(inst,_,_)) -> inst <> "acc")
   |> List.map (fun (i, (op,n,calls)) -> 
     match op with
     | "jmp" -> execute (replace i ("nop",n,calls) seq) 0 0
     | "nop" -> execute (replace i ("jmp",n,calls) seq) 0 0
     | _ -> failwithf "Unexpected operation %s" op)
   |> List.find (fun (_,exitCode) -> exitCode = 0)
  acc

let seq =
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.map (fun x -> 
    match x.Split(" ") with 
    |[|op;n|] -> (op,n |> int,0)
    | _ -> failwithf "unexpected string %s" x)
  |> Seq.toList

printfn "%d" (solveA seq)
printfn "%d" (solveB seq)
