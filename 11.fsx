let occupiedNeighbors (r,c) grid = 
  let gridSize = List.length grid
  [for rt in [r-1..r+1] -> [for ct in [c-1..c+1] -> (rt,ct)]]
  |> List.fold (@) List.empty
  |> List.filter ((fun (x,y) -> x = r && y = c) >> not)
  |> List.filter (fun (x,y) -> x >= 0 && y >= 0)
  |> List.filter (fun (x,y) -> x < gridSize && y < gridSize)
  |> List.map (fun (x,y) -> List.item x grid |> List.item y)
  |> List.sumBy (fun x -> if x = '#' then 1 else 0)

let rec move grid = 
  let newGrid = List.mapi (fun r row -> 
    List.mapi (fun c pos -> 
      match pos with 
      | '.' -> pos
      | '#' -> if (occupiedNeighbors (r,c) grid) <= 3 then '#' else 'L'
      | 'L' -> if (occupiedNeighbors (r,c) grid) = 0 then '#' else 'L'
      | _ ->  failwithf "Unexpected map item %c" pos) row) grid
  if newGrid = grid then grid else move newGrid

let solveA (grid:list<list<char>>) =
  List.map (fun row -> List.map (fun pos -> if pos = 'L' then '#' else pos) row) grid
  |> move
  |> List.sumBy (fun row -> List.sumBy (fun pos -> if pos = '#' then 1 else 0) row)

let occupiedInRange (r,p) grid = 
  let up = [r-1..-1..0]
  let right = [p+1..((List.item 0 grid |> List.length) - 1)]
  let left = [p-1..-1..0]
  let down = [r+1..((List.length grid) - 1)]

  let upRight = Seq.zip up right |> Seq.toList
  let downRight = Seq.zip down right |> Seq.toList
  let downLeft = Seq.zip down left |> Seq.toList
  let upLeft = Seq.zip up left |> Seq.toList

  [
    List.map (fun row -> List.item row grid |> List.item p) up;
    List.map (fun (x,y) -> List.item x grid |> List.item y) upRight;
    List.map (fun col -> List.item r grid |> List.item col) right;
    List.map (fun (x,y) -> List.item x grid |> List.item y) downRight;
    List.map (fun row -> List.item row grid |> List.item p) down;
    List.map (fun (x,y) -> List.item x grid |> List.item y) downLeft;
    List.map (fun col -> List.item r grid |> List.item col) left;
    List.map (fun (x,y) -> List.item x grid |> List.item y) upLeft;
  ] 
  |> List.map (fun dir -> List.tryFind (fun x -> x = 'L' || x = '#') dir)
  |> List.sumBy (fun x -> match x with | Some '#' -> 1 | _ -> 0)

let rec moveB grid = 
  let newGrid = List.mapi (fun r row -> 
    List.mapi (fun p pos -> 
      match pos with 
      | '.' -> pos
      | '#' -> if (occupiedInRange (r,p) grid) <= 4 then '#' else 'L'
      | 'L' -> if (occupiedInRange (r,p) grid) = 0 then '#' else 'L'
      | _ ->  failwithf "Unexpected map item %c" pos) row) grid
  if newGrid = grid then grid else moveB newGrid

let solveB (grid:list<list<char>>) =
  List.map (fun row -> List.map (fun pos -> if pos = 'L' then '#' else pos) row) grid
  |> moveB
  |> List.sumBy (fun row -> List.sumBy (fun pos -> if pos = '#' then 1 else 0) row)

let input =
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.map Seq.toList
  |> Seq.toList

printfn "%d" (solveA input)
printfn "%d" (solveB input)
