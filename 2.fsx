open System.IO
open System.Text.RegularExpressions

let solveA (seq:list<int * int * char * string>) =
  seq
  |> List.filter 
    (fun (min,max,c,(password:string))-> 
      Seq.toList password
      |> Seq.sumBy (fun p -> match p = c with | true -> 1 | false -> 0)
      |> (fun l -> l >= min && l <= max)
    )
  |> List.length

let solveB (seq:list<int * int * char * string>) =
  List.filter 
    (fun (min,max,c,(password:string))-> 
      Seq.toList password
      |> (fun (pwd) -> [(Seq.item (min-1) pwd);(Seq.item (max-1) pwd)])
      |> Seq.filter (fun p -> p = c)
      |> Seq.length
      |> (fun len -> len = 1)
    )
    seq
  |> List.length

let seq = 
  let pattern = "(\d+)-(\d+)\s(\w):\s(\w+)"
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.map 
    ((fun x -> Regex.Match(x, pattern).Groups) 
    >> (fun x -> (Seq.toList x))
    >> (fun x -> 
    match x with 
    | _::min::max::c::password::_ -> (min.Value |> int, max.Value |> int, c.Value |> char, password.Value)
    | _ -> failwith "Wrong number of parts"
    ))
  |> Seq.toList

printfn "%d" (solveA seq)
printfn "%d" (solveB seq)