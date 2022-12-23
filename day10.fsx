open System

let testInput = "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"

let input = "noop
addx 26
addx -21
addx 2
addx 3
noop
noop
addx 23
addx -17
addx -1
noop
noop
addx 7
noop
addx 3
addx 1
noop
noop
addx 2
noop
addx 7
noop
addx -12
addx 13
addx -38
addx 5
addx 34
addx -2
addx -29
addx 2
addx 5
addx 2
addx 3
addx -2
addx -1
addx 8
addx 2
addx 6
addx -26
addx 23
addx -26
addx 33
addx 2
addx -37
addx -1
addx 1
noop
noop
noop
addx 5
addx 5
addx 3
addx -2
addx 2
addx 5
addx 5
noop
noop
addx -2
addx 4
noop
noop
noop
addx 3
noop
noop
addx 7
addx -1
addx -35
addx -1
addx 5
addx 3
noop
addx 4
noop
noop
noop
noop
noop
addx 5
addx 1
noop
noop
noop
addx -7
addx 12
addx 2
addx 7
noop
addx -2
noop
noop
addx 7
addx 2
addx -39
noop
noop
addx 5
addx 2
addx -4
addx 25
addx -18
addx 7
noop
addx -2
addx 5
addx 2
addx 6
addx -5
addx 2
addx -22
addx 29
addx -21
addx -7
addx 31
addx 2
noop
addx -36
addx 1
addx 5
noop
addx 1
addx 4
addx 5
noop
noop
noop
addx 3
noop
addx -13
addx 15
noop
addx 5
noop
addx 1
noop
addx 3
addx 2
addx 4
addx 3
noop
addx -3
noop"

type Opcode = 
  | NOOP
  | ADDX of int

let parseLine (s: string) =
  match s.[0..3] with
  | "noop" -> NOOP
  | "addx" ->
    let value = Int32.Parse s.[4..]
    ADDX value
  | _ -> failwith "Invalid opcode"

let parsedInput =
  input.Split("\n")
  |> Array.map parseLine

let simulateOpcode (registerX: int) opcode =
  match opcode with
  | NOOP ->
    (registerX, 1)
  | ADDX i ->
    (registerX + i, 2)

let part1 =
  let targetCycles = [20; 60; 100; 140; 180; 220]
  ((1, 1, targetCycles, 0), parsedInput |> Array.indexed)
  ||> Array.fold (fun (cycle, x, targetCycles, sumSignal) (i, opcode) ->
    let newx, cost = simulateOpcode x opcode
    let targetCycle = targetCycles |> List.tryHead |> Option.defaultValue Int32.MaxValue
    if (cycle + cost > targetCycle) then
      printfn "x: %A, opcode: %A, index: %A, cycle: %A" x opcode i cycle
      (cycle + cost, newx, targetCycles |> List.tail, sumSignal + (targetCycle * x))
    else
      (cycle + cost, newx, targetCycles, sumSignal)
  )

let part2 =
  let getPixel x cycle =
    let col = (cycle-1) % 40
    if abs (x - col) <= 1 then
      'X'
    else
      ' '
  
  (([], 1, 1), parsedInput)
  ||> Array.fold (fun (output, cycle, x) opcode ->
    let newx, cost = simulateOpcode x opcode
    let chars = 
      [0..cost-1]
      |> List.map (fun i -> getPixel x (cycle+i))
    (List.append output chars, cycle+cost, newx)
  )
  |> fun (o, _, _) -> o
  |> List.chunkBySize 40
  |> List.map (List.toArray >> String)
  |> String.concat "\n"

printfn "part1: %A" part1
printfn "part2: \n%s" part2 // BZPAJELK