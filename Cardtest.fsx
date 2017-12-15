type Card (ID : int, value : int, name : string) =
    member this.CardID() = ID
    member this.Value = value
    member this.Name = name

let eshjerte = new Card(1,1,"eshjerte")
let tohjerte = new Card(2,2,"tohjerte")
let trehjerte = new Card(3,3,"trehjerte")
let firehjerte = new Card(4,4,"firehjerte")
let femhjerte = new Card(5,5,"femhjerte")
let sekshjerte = new Card(6,6,"sekshjerte")
let syvhjerte = new Card(7,7,"syvhjerte")
let ottehjerte = new Card(8,8,"ottehjerte")
let nihjerte = new Card(9,9,"nihjerte")
let tihjerte = new Card(10,10,"tihjerte")
let knægthjerte = new Card(11,10,"knægthjerte")
let damehjerte = new Card(12,10,"damehjerte")
let kongehjerte = new Card(13,10,"kongehjerte")
let esrude = new Card(14,1,"esrude")
let torude = new Card(15,2,"torude")
let trerude = new Card(16,3,"trerude")
let firerude = new Card(17,4,"firerude")
let femrude = new Card(18,5,"femrude")
let seksrude = new Card(19,6,"seksrude")
let syvrude = new Card(20,7,"syvrude")
let otterude = new Card(21,8,"otterude")
let nirude = new Card(22,9,"nirude")
let tirude = new Card(23,10,"tirude")
let knægtrude = new Card(24,10,"knægtrude")
let damerude = new Card(25,10,"damerude")
let kongerude = new Card(26,10,"kongerude")
let esspar = new Card(27,1,"esspar")
let tospar = new Card(28,2,"tospar")
let trespar = new Card(29,3,"trespar")
let firespar = new Card(30,4,"firespar")
let femspar = new Card(31,5,"femspar")
let seksspar = new Card(32,6,"seksspar")
let syvspar = new Card(33,7,"syvspar")
let ottespar = new Card(34,8,"ottespar")
let nispar = new Card(35,9,"nispar")
let tispar = new Card(36,10,"tispar")
let knægtspar = new Card(37,10,"knægtspar")
let damespar = new Card(38,10,"damespar")
let kongespar = new Card(39,10,"kongespar")
let esspade = new Card(40,1,"esspade")
let tospade = new Card(41,2,"tospade")
let trespade = new Card(42,3,"trespade")
let firespade = new Card(43,4,"firespade")
let femspade = new Card(44,5,"femspade")
let seksspade = new Card(45,6,"seksspade")
let syvspade = new Card(46,7,"syvspade")
let ottespade = new Card(47,8,"ottespade")
let nispade = new Card(48,9,"nispade")
let tispade = new Card(49,10,"tispade")
let knægtspade = new Card(50,10,"knægtspade")
let damespade = new Card(51,10,"damespade")
let kongespade = new Card(52,10,"kongespade")

let mutable deck = [|eshjerte;tohjerte;trehjerte;firehjerte;femhjerte;sekshjerte;syvhjerte;ottehjerte;nihjerte;tihjerte;knægthjerte;damehjerte;kongehjerte;esrude;torude;trerude;firerude;femrude;seksrude;syvrude;otterude;nirude;tirude;knægtrude;damerude;kongerude;esspar;tospar;trespar;firespar;femspar;seksspar;syvspar;ottespar;nispar;tispar;knægtspar;damespar;kongespar;esspade;tospade;trespade;firespade;femspade;seksspade;syvspade;ottespade;nispade;tispade;knægtspade;damespade;kongespade|]
let gen = System.Random()


let CardDraw (x : int) = 
    //giver spilleren kortet soon tm, GiveCard funktionen skal give kortet.
    let returncard = (deck.[x])
    // Fjerner element fra array
    deck <- deck |> Array.filter ((<>)deck.[x])
    returncard

    
let Randomizer() =
    let x = gen.Next(0, deck.Length) //indtil sidste indeks som ved starten er 51. 
    CardDraw (x)

type Player() =
    //let mutable isBust() = false
    let mutable handValue = 0
    let mutable hand : Card [] = [||]
    //member this.IsBust () = isBust
    member this.Handvalue() = handValue
    member this.Hand() = hand
    member this.Hit () = 
        // træk et kort
        let newCard = Randomizer()
        //tilføjer kortet til hånden
        hand <- (Array.append hand [|newCard|])
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

        //isBust <- true
        // spillet slutter!

        // ellers læg værdien til den samlede værdi
        // næste spilers tur
        //nextTurn ()
        
(*let Isabella = new Player() 
for j = 0 to 5 do
    Isabella.Hit()
for i = 0 to Isabella.Hand().Length-1 do
    printfn "%A" (Isabella.Hand().[i])
*)

//Implemantation af spillet:
//skal initialisere spillet, 

//user input antallet af spillere

type Dealer() =
    inherit Player()
    