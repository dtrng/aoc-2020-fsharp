open System.Text.RegularExpressions

type Waypoint = { X: int; Y: int}
type ShipPosition = { X: int; Y: int}
type WaypointInstruction = | North | South | West | East | Right | Left
type ShipInstruction = | Forward
type Instruction = | WaypointInstruction of WaypointInstruction | ShipInstruction of ShipInstruction

let rec turn heading dir degrees = 
  if degrees = 0 then 
    heading
  else
    match heading with
    | 'N' -> if dir = 'R' then turn 'E' dir (degrees-90) else turn 'W' dir (degrees-90)
    | 'S' -> if dir = 'R' then turn 'W' dir (degrees-90) else turn 'E' dir (degrees-90)
    | 'E' -> if dir = 'R' then turn 'S' dir (degrees-90) else turn 'N' dir (degrees-90)
    | 'W' -> if dir = 'R' then turn 'N' dir (degrees-90) else turn 'S' dir (degrees-90)
    | _ -> failwithf "unknown heading when turning %c" heading

let moveForward heading dist x y =
  match heading with
  | 'N' -> (heading, x, y + dist)
  | 'S' -> (heading, x, y - dist)
  | 'E' -> (heading, x + dist, y)
  | 'W' -> (heading, x - dist, y)
  | _ -> failwithf "unknown heading when going forward %c" heading


let solveA (instr:list<char*int>) =
  let (_, x, y) = 
    List.fold (fun (heading, x, y) (dir,dist) -> 
    match dir with
    | 'N' -> (heading, x, y + dist)
    | 'S' -> (heading, x, y - dist)
    | 'E' -> (heading, x + dist, y)
    | 'W' -> (heading, x - dist, y)
    | 'F' -> moveForward heading dist x y
    | 'L' | 'R' -> (turn heading dir dist, x, y)
    | _ -> failwithf "unknown direction %c" dir
    ) ('E', 0, 0) instr 
  abs x + abs y

let rec rotateWaypoint waypoint dir degrees =
  if (degrees = 0) 
    then waypoint
  else
    match dir with
    | Left -> rotateWaypoint {Waypoint.X = -waypoint.Y; Waypoint.Y = waypoint.X} dir (degrees - 90)
    | Right -> rotateWaypoint {Waypoint.X = waypoint.Y; Waypoint.Y = -waypoint.X} dir (degrees - 90)
    | _ -> failwithf "Invalid turn operation %O" dir


let updateWaypoint (inst,dist) waypoint =
  match inst with
  | North -> {waypoint with Waypoint.Y = waypoint.Y + dist} 
  | South -> {waypoint with Waypoint.Y = waypoint.Y - dist} 
  | East -> {waypoint with Waypoint.X = waypoint.X + dist} 
  | West -> {waypoint with Waypoint.X = waypoint.X - dist} 
  | Left | Right -> rotateWaypoint waypoint inst dist

let moveShip (inst,dist) shipPosition (waypoint: Waypoint) =
  match inst with
  | Forward -> { ShipPosition.X = shipPosition.X + dist * waypoint.X; ShipPosition.Y = shipPosition.Y + dist * waypoint.Y}

let solveB (instr:list<char*int>) =
  List.map (fun (inst,dist)->
    match inst with
    | 'N' -> (North |> WaypointInstruction, dist) 
    | 'S' -> (South |> WaypointInstruction, dist)
    | 'E' -> (East |> WaypointInstruction, dist)
    | 'W' -> (West |> WaypointInstruction, dist)
    | 'L' -> (Left |> WaypointInstruction, dist)
    | 'R' -> (Right |> WaypointInstruction, dist)
    | 'F' -> (Forward |> ShipInstruction, dist)
    | _ -> failwithf "Unknown instruction %c" inst
  ) instr
  |> List.fold (fun (waypoint, shipPosition) (inst, dist) -> 
    match inst with
    | WaypointInstruction x -> (updateWaypoint (x,dist) waypoint, shipPosition)
    | ShipInstruction x -> (waypoint, moveShip (x,dist) shipPosition waypoint)
  ) ({Waypoint.X = 10; Waypoint.Y = 1}, {ShipPosition.X = 0; ShipPosition.Y = 0})
  |> (fun (_, shipPosition) -> abs shipPosition.X + abs shipPosition.Y)

let input = 
  let pattern = "(\w)(\d+)"
  stdin.ReadToEnd() 
  |> (fun x -> x.Trim().Split("\n"))
  |> Seq.map 
    ((fun x -> Regex.Match(x, pattern).Groups) 
    >> (fun x -> (Seq.toList x))
    >> (fun x -> 
      match x with 
      | _::dir::dist::_ -> (dir.Value |> char, dist.Value |> int)
      | _ -> failwithf "Invalid input line %A" x
    ))
  |> Seq.toList

printfn "%d" (solveA input)
printfn "%d" (solveB input)
