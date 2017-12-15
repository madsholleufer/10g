printfn "How many players? "
let PlayerNumber = int (System.Console.ReadLine())
match PlayerNumber with
| x when x < 1 -> failwith "Too few players"
| x when x > 5 -> failwith "Too many players"
| _ -> "TIME TO GO"

//let playergenerator = 

for i = 1 to PlayerNumber do
    let istring = (i.ToString())
    let mutable name = String.concat "" ["player"; istring]
    //let player = new Player() 
    printfn "%A" (name)
