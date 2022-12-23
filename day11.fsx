open System
type MonkeyId = MonkeyId of int

type Monkey = {
  id: MonkeyId
  items: Int64 list
  operation: Int64 -> Int64
  test: Int64 -> MonkeyId
  inspectCounter: int
}

let test (d: Int64) a b (x: Int64) =
  if x % d = 0 then
    MonkeyId a
  else
    MonkeyId b

let input = [
  {
    id = MonkeyId 0
    items = [83; 97; 95; 67]
    operation = (*) (19L)
    test = test (17) 2 7
    inspectCounter = 0
  }
  {
    id = MonkeyId 1
    items = [71; 70; 79; 88; 56; 70]
    operation = (+) (2L)
    test = test 19 7 0
    inspectCounter = 0
  }
  {
    id = MonkeyId 2
    items = [98; 51; 51; 63; 80; 85; 84; 95]
    operation = (+) (7L)
    test = test 7 4 3
    inspectCounter = 0
  }
  {
    id = MonkeyId 3
    items = [77; 90; 82; 80; 79]
    operation = (+) (1L)
    test = test 11 6 4
    inspectCounter = 0
  }
  {
    id = MonkeyId 4
    items = [68]
    operation = (*) (5L)
    test = test 13 6 5
    inspectCounter = 0
  }
  {
    id = MonkeyId 5
    items = [60; 94]
    operation = (+) (5L)
    test = test 3 1 0
    inspectCounter = 0
  }
  {
    id = MonkeyId 6
    items = [81; 51; 85]
    operation = fun x -> x * x
    test = test 5 5 1
    inspectCounter = 0
  }
  {
    id = MonkeyId 7
    items = [98; 81; 63; 65; 84; 71; 84]
    operation = (+) (3L)
    test = test 2 2 3
    inspectCounter = 0
  }
] // 36 items


let inspect (monkey: Monkey) =
  { monkey with 
      items = monkey.items |> List.map monkey.operation
      inspectCounter = monkey.inspectCounter + (monkey.items |> List.length)
  }

let relief (monkey: Monkey) =
  { monkey with 
      items = monkey.items |> List.map (fun x -> x / 3L)
  }

let relief2 (monkey: Monkey) =
  { monkey with 
      items = monkey.items |> List.map (fun x -> x % 9699690L)
  }

let testItems (monkey: Monkey) =
  let thrownItems = 
    monkey.items
    |> List.map (fun x -> (monkey.test x, x)) 
    |> List.groupBy fst

  ({monkey with items = []}, thrownItems)

let turn = inspect >> relief >> testItems

let turn2 = inspect >> relief2 >> testItems

let round turnfn (monkeys: Monkey list)  =
  (monkeys, [0..(monkeys |> List.length) - 1])
  ||> List.fold (fun mx i ->
    let monkey = mx.[i]
    let (newMonkey, thrownItems) = turnfn monkey

    let newMonkeys =
      (mx, thrownItems)
      ||> List.fold (fun mmx items ->
        let (MonkeyId id, items) = items
        let monkey = mmx.[id]
        let itemsToAdd = items |> List.map snd
        let newMonkey = {monkey with items = List.append monkey.items itemsToAdd}
        List.updateAt id newMonkey mmx
      )
    
    List.updateAt i newMonkey newMonkeys
  )

let part1 () =
  ( input, [0..19])
  ||> List.fold (fun x _ -> round turn x)
  |> List.sortByDescending (fun m -> m.inspectCounter)
  |> List.take 2
  |> function
     | [a; b] -> a.inspectCounter*b.inspectCounter
     | _ -> failwith "bla"

let part2 () =
  ( input, [0..9999])
  ||> List.fold (fun x _ -> round turn2 x)
  |> List.sortByDescending (fun m -> m.inspectCounter)
  |> List.take 2
  |> function
     | [a; b] -> (bigint a.inspectCounter) * (bigint b.inspectCounter)
     | _ -> failwith "bla"

printfn "part1: %A" (part1 ())
printfn "part1: %A" (part2 ())
