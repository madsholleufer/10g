type Player =
    let mutable isBust = false
    let mutable handValue = 0
    let mutable hand : Card = [||]
    //member this.IsBust () = isBust
    //member this.Stand () = nextTurn ()
    member this.Hit () = 
        // træk et kort
        let newCard = Randomizer()
        //tilføjer kortet til hånden
        Array.concat [ hand ; [|newCard|]]
        // opdaterer den samlede værdi
        handValue <- handValue + newCard.Value
        (*
        Hvis det er et es, man har trukket:
        handValue <- handValue + newCard
        if (newCard.Value = 1)
            eshandValue <- handValue + 10
        *)
        // hvis det nye kort medfører at den samlede værdi er 
        // over 21, så

        isBust <- true
        // spillet slutter!

        // ellers læg værdien til den samlede værdi
        handValue <- handValue + value
        nextTurn ()
