open System.IO
open System.Text.RegularExpressions

let solveA (seq:list<string>) =
  List.filter (fun (x:string) -> 
    x.Contains("byr:") 
    && x.Contains("iyr:") 
    && x.Contains("eyr:") 
    && x.Contains("hgt:") 
    && x.Contains("hcl:") 
    && x.Contains("ecl:") 
    && x.Contains("pid:") 
    ) seq
  |> Seq.length

let (|Prefix|_|) (p:string) (input:string) =
  if input.StartsWith(p) then
    Some(input.Substring(p.Length))
  else 
    None

let (|Suffix|_|) (p:string) (input:string) =
  if input.EndsWith(p) then
    Some(input.Substring(0, input.Length - p.Length))
  else 
    None

let isValidByr input =
  input |> int |> (fun n -> n >= 1920 && n <= 2002)

let isValidIyr input =
  input |> int |> (fun n -> n >= 2010 && n <= 2020)

let isValidEyr input =
  input |> int |> (fun n -> n >= 2020 && n <= 2030)

let isValidHgt input =
  match input with
  | Suffix "cm" value -> value |> int |> (fun n -> n >= 150 && n <= 193)
  | Suffix "in" value -> value |> int |> (fun n -> n >= 59 && n <= 76)
  | _ -> false

let isValidHcl input =
  let pattern = "^#[0-9a-f]{6}$"
  Regex.IsMatch(input,pattern)

let isValidEcl input =
  match input with
  | "amb"
  | "blu"
  | "brn"
  | "gry"
  | "grn"
  | "hzl"
  | "oth" -> true
  | _ -> false

let isValidPid input =
  let pattern = "^[0-9]{9}$"
  Regex.IsMatch(input,pattern)

let solveB (seq:list<string>) =
  List.filter (fun (x:string) -> 
    x.Contains("byr:") 
    && x.Contains("iyr:") 
    && x.Contains("eyr:") 
    && x.Contains("hgt:") 
    && x.Contains("hcl:") 
    && x.Contains("ecl:") 
    && x.Contains("pid:") 
    ) seq
  |> List.map (fun x -> x.Trim().Split(" "))
  |> List.filter (fun x ->
    Seq.map (fun s -> 
      match s with
      | Prefix "byr:" value -> isValidByr value
      | Prefix "iyr:" value -> isValidIyr value
      | Prefix "eyr:" value -> isValidEyr value
      | Prefix "hgt:" value -> isValidHgt value
      | Prefix "hcl:" value -> isValidHcl value
      | Prefix "ecl:" value -> isValidEcl value
      | Prefix "pid:" value -> isValidPid value
      | Prefix "cid:" _ -> true
      | _ -> failwithf "%O" s
      ) x
    |> Seq.exists not |> not)
  |> Seq.length

let seq =
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim())
  |> (fun file -> file.Split "\n\n")
  |> Seq.map (fun doc -> doc.Replace( '\n', ' '))
  |> Seq.toList

printfn "%d" (solveA seq)
printfn "%d" (solveB seq)