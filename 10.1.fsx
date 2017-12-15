type Car (effec : float) =
    let mutable benzin = 0.
    member this.Effec = effec 
    member this.AddGas(b:float) = benzin <- benzin + b
    member this.GasLeft() = benzin
    member this.drive(dist) = if benzin < dist/effec then failwith "Idiot du har ikke nok benzin" else benzin <- benzin - dist/effec  

let vroom = new Car(10.)
vroom.AddGas(15.)
vroom.drive(10.)
printfn "Der er så mange liter benzin skeet skeet %A" (vroom.GasLeft())

let vroom2 = new Car(10.)
vroom2.AddGas(15.)
vroom2.drive(145.)
printfn "Der er så mange liter benzin skeet skeet %A" (vroom2.GasLeft())