// Kort klasse, der indeholder kortets værdi, ID samt et navn på kortet.
type Card (ID : int, cardvalue : int, name : string) =
    let mutable value = cardvalue
    member this.CardID() = ID
    member this.Value = value
    // Gør det muligt at overskrive kortets værdi. Bruges når kortet er et es, hvilket kan være 1 eller 11.
    member this.SetValue(newvalue : int) = value <- newvalue
    member this.Name = name
    
// Definerer kortene
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

// Definerer dækket
let mutable deck = [|eshjerte;tohjerte;trehjerte;firehjerte;femhjerte;sekshjerte;syvhjerte;ottehjerte;nihjerte;tihjerte;knægthjerte;damehjerte;kongehjerte;esrude;torude;trerude;firerude;femrude;seksrude;syvrude;otterude;nirude;tirude;knægtrude;damerude;kongerude;esspar;tospar;trespar;firespar;femspar;seksspar;syvspar;ottespar;nispar;tispar;knægtspar;damespar;kongespar;esspade;tospade;trespade;firespade;femspade;seksspade;syvspade;ottespade;nispade;tispade;knægtspade;damespade;kongespade|]
// Til at generere et tilfældigt tal
let gen = System.Random()

// Funktion der trækker et kort fra dækket. Den kaldes i Randomizer() funktionen, se nedenfor.
let CardDraw (x : int) = 
    // trækker et kort
    let returncard = (deck.[x])
    // Fjerner kortet fra dækket
    deck <- deck |> Array.filter ((<>)deck.[x])
    returncard

// Funktion der genererer et tal mellem 0 og 51 inklusive, og som kalder funktionen CardDraw()
// for at trække et kort fra dækket på det index i dæk arrayet.
let Randomizer() =
    let x = gen.Next(0, deck.Length) // indtil sidste indeks som ved starten er 51. 
    CardDraw (x)

// Spiller klasse, som har en korthånd, en korthåndværdi samt nogle metoder, der beskrives nedenfor.
type Player() =
    let mutable handValue = 0
    let mutable hand : Card [] = [||] // korthånden er et array af kort
    // metode, der returnerer korthåndværdien
    member this.Handvalue() = handValue
    // metode, der returnerer korthånden
    member this.Hand() = hand
    // metode, der giver spilleren et kort. Metoden kalder Randomizer() funktionen.
    member this.Hit () = 
        // trækker et kort vha. Randomizer()
        let newCard = Randomizer()
        // tilføjer kortet til hånden
        hand <- (Array.append hand [|newCard|])
        // Hvis kortet er et es og værdien af korthånden er mindre end 11, så bruges et es som 11
        if newCard.Value = 1 && handValue < 11 then
            hand.[hand.Length-1].SetValue(11) //opdaterer værdien af det nye kort til at være 11
            handValue <- handValue + 11 //tilføjer til korthåndværdien
        else // hvis korthånden er 11 eller større bruges et es som 1
            handValue <- handValue + newCard.Value

type Dealer() =
    inherit Player()
    
printfn "Hvor mange spillere? "
let PlayerNumber = int (System.Console.ReadLine())
match PlayerNumber with
| x when x < 1 -> failwith "Too few players"
| x when x > 5 -> failwith "Too many players"
| _ -> printfn "TIME TO GO"
// Laver et tomt Player array
let playerarray : Player array = Array.zeroCreate PlayerNumber 
//overskriver indekset for hver spiller med et nyt player objekt.
for i = 0 to PlayerNumber-1 do //Opretter player objekter
    playerarray.[i] <- new Player()
    for j = 0 to 1 do //kalder hit metoden to gange for at hver spiller starter med 2 kort
        playerarray.[i].Hit()

// AI
printfn "Hvor mange AI? "
let AINumber = int (System.Console.ReadLine())
match AINumber with
| x when x < 1 -> failwith "Too few AI"
| x when x > 5 -> failwith "Too many AI"
| _ -> printfn "TIME TO GO"
// Laver et tomt Player array
let AIarray : Player array = Array.zeroCreate AINumber 
//overskriver indekset for hver spiller med et nyt player objekt.
for i = 0 to AINumber-1 do //Opretter player objekter
    AIarray.[i] <- new Player()
    for j = 0 to 1 do //kalder hit metoden to gange for at hver spiller starter med 2 kort
        AIarray.[i].Hit()

// Definerer dealer og giver dealer to kort
let dealer = Dealer()
for j = 0 to 1 do //kalder hit metoden to gange for at dealer starter med 2 kort
    dealer.Hit()

// Funktion der viser kortenes værdier
let PrintHands() = 
    // Printer spillernes hænder
    for i = 0 to PlayerNumber-1 do
        printfn "Player %is hånd: " (i+1)
        let mutable handlength = playerarray.[i].Hand().Length    
        for j = 0 to handlength-1 do
            printf "%A, værdi: %A \t" (playerarray.[i].Hand().[j].Name) (playerarray.[i].Hand().[j].Value)
        printfn ""
    // Printer AI hænder
    for l = 0 to AINumber-1 do
        printfn "AI nummer %is hånd: " (l+1)
        let mutable AIHand = AIarray.[l].Hand().Length
        for j = 0 to AIHand-1 do
            printf "%A, værdi: %A \t" (AIarray.[l].Hand().[j].Name) (AIarray.[l].Hand().[j].Value)
        printfn ""
    // Printer dealers hånd
    printfn "Dealers hånd:"
    for k = 0 to dealer.Hand().Length-1 do
        printf "%A, værdi: %A \t" (dealer.Hand().[k].Name) (dealer.Hand().[k].Value)
    printfn ""

// Spillernes tur (players)
for i = 0 to PlayerNumber-1 do
    PrintHands()
    printfn "Player %i's tur" (i+1)
    printfn "HIT or STAND??"
    let mutable stand = false
    while not stand do
        match System.Console.ReadLine() with
        | "HIT" -> playerarray.[i].Hit() 
                   PrintHands()
                   if playerarray.[i].Handvalue() > 21 then
                    stand <- true
                    printfn "YOU ARE BUST"
        | "STAND" -> printfn "sleep tight"
                     stand <- true
        | _ -> printfn "Wrong Input"

//AI tur
printfn "AIs tur"
for i = 0 to AIarray.Length-1 do
    while AIarray.[i].Handvalue() < 17 do
        AIarray.[i].Hit()
printfn "AIs hånd:"
for i = 0 to AIarray.Length-1 do
    printf "AI nummer %A:" i
    for j = 0 to AIarray.[i].Hand().Length-1 do
        printf "%A, værdi: %A \t" (AIarray.[i].Hand().[j].Name) (AIarray.[i].Hand().[j].Value)
    printfn ""

//Dealers tur
printfn "Dealers tur"
while dealer.Handvalue() < 17 do
    dealer.Hit()
 
printfn "Dealers hånd:"
for k = 0 to dealer.Hand().Length-1 do
    printf "%A, værdi: %A \t" (dealer.Hand().[k].Name) (dealer.Hand().[k].Value)
printfn ""

if dealer.Handvalue() > 21 then
    printfn "DEALER IS BUST"
// Players: Hvem har vundet?
for i = 0 to PlayerNumber-1 do
    if playerarray.[i].Handvalue() < 22 && dealer.Handvalue() < 22 && playerarray.[i].Handvalue() > dealer.Handvalue() || dealer.Handvalue() > 21 && playerarray.[i].Handvalue() < 22 then
        printfn "Player %i har vundet! :-)" (i+1)
    else 
        printfn "Player %i har tabt! :-(" (i+1)
// AIs: Hvem har vundet?
for i = 0 to AINumber-1 do
    if AIarray.[i].Handvalue() < 22 && dealer.Handvalue() < 22 && AIarray.[i].Handvalue() > dealer.Handvalue() || AIarray.[i].Handvalue() < 22 && dealer.Handvalue() > 21  then
        printfn "AI %i har vundet! :-)" (i+1)
    else 
        printfn "AI %i har tabt! :-(" (i+1)
