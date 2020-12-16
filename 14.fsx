open System
open System.IO
open System.Text.RegularExpressions

type MaskInstr = {Pattern: string}
type MemInstr = {Address: int; Value: int64}
type Instr = 
  | MaskInstr of MaskInstr 
  | MemInstr of MemInstr

let bitmaskToAndOr (mask:string) =
  Seq.toList mask
  |> Seq.mapi (fun i m -> (i,m))
  |> Seq.fold (fun (a,o) (i,m) -> 
    match m with
    | 'X' -> (a,o)
    | '1' -> (a,o + (pown 2L (35-i)))
    | '0' -> (a - (pown 2L (35-i)),o)
    | _ -> failwithf "Unexpected mask value %c" m
  ) ((pown 2L 36)-1L, 0L)

let solveA (input:list<Instr>) =
  let mutable a = 0L
  let mutable o = 0L
  let mutable ao = (0L,0L)
  let mutable map = Map.empty<int,int64>
  for instr in input do
    match instr with
    | MaskInstr mask -> ao <- bitmaskToAndOr mask.Pattern
    | MemInstr mem -> map <- Map.change mem.Address (fun _ -> 
      let (a,o) = ao
      Some ((mem.Value &&& a) ||| o)) map
  Map.fold (fun sum k v -> sum + v) 0L map

let addressMask mask mem =
  let binAddress = Convert.ToString(mem.Address,2).PadLeft(36,'0')
  let out = 
    List.zip (Seq.toList binAddress) (Seq.toList mask.Pattern)
    |> List.map (fun (addr,mask) -> 
      match mask with
      | '0' -> addr
      | '1' -> mask
      | 'X' -> mask
      | _ -> failwithf "Unexpected mask %c" mask
    )
  {Pattern = String.Join("", out)}

let isOverlapping mask1 mask2 =
  if Seq.length mask1.Pattern <> Seq.length mask2.Pattern then printfn "Not same length %s %s" mask1.Pattern mask2.Pattern
  List.zip (mask1.Pattern |> Seq.toList) (mask2.Pattern |> Seq.toList)
  |> List.filter (fun (c1,c2) -> c1 = c2 || c1 = 'X' || c2 = 'X')
  |> List.length
  |> (fun l -> l = (mask1.Pattern.Length))

let getSubPatterns pattern mask =
  Seq.mapi (fun i c -> (i,c)) pattern.Pattern
  |> Seq.fold (fun (pat, sub) (i,c) -> 
    match (c, Seq.item i mask.Pattern) with
    | ('X','0') -> (Seq.mapi (fun ii cc -> if ii = i then '0' else cc) pat, Seq.mapi (fun ii cc -> if ii = i then '1' else cc) pat::sub)
    | ('X','1') -> (Seq.mapi (fun ii cc -> if ii = i then '1' else cc) pat, Seq.mapi (fun ii cc -> if ii = i then '0' else cc) pat::sub)
    | _ -> (pat, sub)
  ) (pattern.Pattern |> Seq.map id, List.empty)
  |> (fun (a, l) -> l)
  |> List.map (fun x -> {Pattern = (x |> String.Concat)})

let unusedCombinations addrMask usedMasks =
  let overlaps = List.filter (fun x -> isOverlapping x addrMask) usedMasks
  let pres = 
    overlaps 
    |> List.fold (fun patterns mask -> 
    List.fold (fun subPatterns pattern ->
      match (isOverlapping pattern mask) with
      | true -> (getSubPatterns pattern mask)@subPatterns
      | false -> pattern::subPatterns
    ) List.empty patterns
    ) [addrMask]
  let res = 
    pres |> List.sumBy (fun x -> x.Pattern |> Seq.filter (fun y -> y = 'X') |> Seq.length |> (fun x -> pown 2L x))
  res

let solveB (seq:list<Instr>) =
  let inta = ({Pattern = "000000000000000000000000000000000000"}, List.empty<MaskInstr*MemInstr>)
  let (_,maskMem) = 
    seq 
    |> List.fold (fun (mask, list) instr ->
      match instr with
      | MaskInstr m -> (m, list)
      | MemInstr mem -> (mask, (addressMask mask mem, mem)::list)
    ) inta
  let (sum,_) =
    // List.rev maskMem
    maskMem
    |> List.fold (fun (sum, usedMasks) (addrMask, mem) -> 
      (
        sum + (unusedCombinations addrMask usedMasks) * mem.Value, 
        addrMask::usedMasks)
      ) (0L, List.empty<MaskInstr>)
  sum

let maskPattern = "mask\s=\s(\w+)"
let memPattern = "mem\[(\d+)\]\s=\s(\d+)"
let input = 
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.map 
    (fun (x:string) -> 
      if Regex.IsMatch(x, maskPattern) then
        match Regex.Match(x, maskPattern).Groups |> Seq.toList with
        | _::mask::_ -> MaskInstr {Pattern = (mask.Value |> string)}
        | _ -> failwithf "Invalid mask %s" x
      else
        match Regex.Match(x, memPattern).Groups |> Seq.toList with
        | _::addr::caa::_ -> MemInstr {Address = (addr.Value |> int); Value = caa.Value |> int64}
        | _ -> failwithf "Invalid mem %s" x
    )
  |> Seq.toList
printfn "%d" (solveA input)
printfn "%d" (solveB input)