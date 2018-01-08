// Kort klasse, der indeholder kortets værdi samt et navn på kortet.
type Card (cardvalue : int, name : string) =
    let mutable value = cardvalue
    member this.Value = value
    // Gør det muligt at overskrive kortets værdi. Bruges når kortet er et es, hvilket kan være 1 eller 11.
    member this.SetValue(newvalue : int) = value <- newvalue
    member this.Name = name
    
// Definerer kortene
let eshjerte = new Card(1,"eshjerte")
let tohjerte = new Card(2,"tohjerte")
let trehjerte = new Card(3,"trehjerte")
let firehjerte = new Card(4,"firehjerte")
let femhjerte = new Card(5,"femhjerte")
let sekshjerte = new Card(6,"sekshjerte")
let syvhjerte = new Card(7,"syvhjerte")
let ottehjerte = new Card(8,"ottehjerte")
let nihjerte = new Card(9,"nihjerte")
let tihjerte = new Card(10,"tihjerte")
let knægthjerte = new Card(10,"knægthjerte")
let damehjerte = new Card(10,"damehjerte")
let kongehjerte = new Card(10,"kongehjerte")
let esrude = new Card(1,"esrude")
let torude = new Card(2,"torude")
let trerude = new Card(3,"trerude")
let firerude = new Card(4,"firerude")
let femrude = new Card(5,"femrude")
let seksrude = new Card(6,"seksrude")
let syvrude = new Card(7,"syvrude")
let otterude = new Card(8,"otterude")
let nirude = new Card(9,"nirude")
let tirude = new Card(10,"tirude")
let knægtrude = new Card(10,"knægtrude")
let damerude = new Card(10,"damerude")
let kongerude = new Card(10,"kongerude")
let esspar = new Card(1,"esspar")
let tospar = new Card(2,"tospar")
let trespar = new Card(3,"trespar")
let firespar = new Card(4,"firespar")
let femspar = new Card(5,"femspar")
let seksspar = new Card(6,"seksspar")
let syvspar = new Card(7,"syvspar")
let ottespar = new Card(8,"ottespar")
let nispar = new Card(9,"nispar")
let tispar = new Card(10,"tispar")
let knægtspar = new Card(10,"knægtspar")
let damespar = new Card(10,"damespar")
let kongespar = new Card(10,"kongespar")
let esklør = new Card(1,"esklør")
let toklør = new Card(2,"toklør")
let treklør = new Card(3,"treklør")
let fireklør = new Card(4,"fireklør")
let femklør = new Card(5,"femklør")
let seksklør = new Card(6,"seksklør")
let syvklør = new Card(7,"syvklør")
let otteklør = new Card(8,"otteklør")
let niklør = new Card(9,"niklør")
let tiklør = new Card(10,"tiklør")
let knægtklør = new Card(10,"knægtklør")
let dameklør = new Card(10,"dameklør")
let kongeklør = new Card(10,"kongeklør")

// Definerer dækket
let mutable deck = [|eshjerte;tohjerte;trehjerte;firehjerte;femhjerte;sekshjerte;syvhjerte;ottehjerte;nihjerte;tihjerte;knægthjerte;damehjerte;kongehjerte;esrude;torude;trerude;firerude;femrude;seksrude;syvrude;otterude;nirude;tirude;knægtrude;damerude;kongerude;esspar;tospar;trespar;firespar;femspar;seksspar;syvspar;ottespar;nispar;tispar;knægtspar;damespar;kongespar;esklør;toklør;treklør;fireklør;femklør;seksklør;syvklør;otteklør;niklør;tiklør;knægtklør;dameklør;kongeklør|]
// Nedenstående bruges til at generere et tilfældigt tal
let gen = System.Random()

(* Funktion der trækker et kort fra dækket. 
    Den kaldes i Randomizer() funktionen, se nedenfor.
    Input til funktionen er et heltal, som er det tal, der genereres 
    i Randomizer(). Funktionen returnerer et kort af typen Card. *)
let CardDraw (x : int) = 
    // trækker et kort vha. indeksering i dæk arrayet.
    let returncard = (deck.[x])
    // Fjerner kortet fra dækket
    deck <- deck |> Array.filter ((<>)deck.[x])
    // returnerer kortet, der blev trukket
    returncard

(* Funktion der genererer et tal, der svarer til et indeks i dæk arrayet. 
    Dette er til at starte med mellem 0 og 51 (inklusive). Funktionen kalder 
    CardDraw() for at trække et kort fra dæk arrayet. *)
let Randomizer() =
    let x = gen.Next(0, deck.Length) // indtil sidste indeks i arrayet
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
            hand.[hand.Length-1].SetValue(11) // opdaterer værdien af det nye kort til at være 11
            handValue <- handValue + 11 // tilføjer til korthåndværdien
        else // hvis korthånden er 11 eller større bruges et es som 1
            handValue <- handValue + newCard.Value

// Dealer klasse. Vi har valgt at inkludere denne klasse hvis nu vi senere hen
// vil tilføje attributter eller metoder til dealeren som en spiller ikke skal have.
type Dealer() =
    inherit Player()

// Selve spillet
printfn "Hvor mange spillere? "
let PlayerNumber = int (System.Console.ReadLine())
match PlayerNumber with
| x when x < 1 -> failwith "For få spillere"
| x when x > 5 -> failwith "For mange spillere"
| _ -> printfn ""
// Laver et tomt Player array
let playerarray : Player array = Array.zeroCreate PlayerNumber 
// Overskriver med et nyt player objekt for hver spiller
for i = 0 to PlayerNumber-1 do 
    playerarray.[i] <- new Player() // Opretter Player objekt
    for j = 0 to 1 do // Kalder hit metoden to gange for at hver spiller starter med 2 kort
        playerarray.[i].Hit()

printfn "Hvor mange AI? "
let AINumber = int (System.Console.ReadLine())
match AINumber with
| x when x < 0 -> failwith "For få AI"
| x when x > 5 -> failwith "For mange AI"
| _ -> printfn ""
// Laver et tomt AI array
let AIarray : Player array = Array.zeroCreate AINumber
// Overskriver med et nyt player objekt for hver AI
for i = 0 to AINumber-1 do // Opretter Player objekt
    AIarray.[i] <- new Player()
    for j = 0 to 1 do // Kalder hit metoden to gange for at hver spiller starter med 2 kort
        AIarray.[i].Hit()

// Definerer en dealer og giver dealeren to kort
let dealer = Dealer()
for j = 0 to 1 do // Kalder hit metoden to gange for at dealeren starter med 2 kort
    dealer.Hit()

// Funktion der printer kortene (navn og værdi) til konsollen
let PrintHands() = 
    // Printer spillernes hænder
    printfn "-------------------------------------------------------------------------------------------"
    for i = 0 to PlayerNumber-1 do
        printfn "Spiller %is hånd: " (i+1)
        let mutable handlength = playerarray.[i].Hand().Length    
        for j = 0 to handlength-1 do
            printf "%A, værdi: %A \t" (playerarray.[i].Hand().[j].Name) (playerarray.[i].Hand().[j].Value)
        printfn "\n"
    // Printer AI hænder
    for l = 0 to AINumber-1 do
        printfn "AI nummer %is hånd: " (l+1)
        let mutable AIHand = AIarray.[l].Hand().Length
        for j = 0 to AIHand-1 do
            printf "%A, værdi: %A \t" (AIarray.[l].Hand().[j].Name) (AIarray.[l].Hand().[j].Value)
        printfn "\n"
    // Printer dealers hånd
    printfn "Dealers hånd:"
    for k = 0 to dealer.Hand().Length-1 do
        printf "%A, værdi: %A \t" (dealer.Hand().[k].Name) (dealer.Hand().[k].Value)
    printfn "\n-------------------------------------------------------------------------------------------"
    printfn "\n"

// Spillernes tur (players)
for i = 0 to PlayerNumber-1 do
    PrintHands() // Først vises alle uddelte kort
    printfn "Spiller %i's tur" (i+1)
    let mutable stand = false
    // Indtil spilleren har valgt stand, skal der enten hittes, ellers er input ugyldigt
    while not stand do
        printfn "HIT eller STAND?"
        match System.Console.ReadLine() with
        | "HIT" -> playerarray.[i].Hit()
                   printfn ""
                   PrintHands()
                   if playerarray.[i].Handvalue() > 21 then // Spiller er bust
                    stand <- true
                    printfn "! Spiller %i er BUST! \n" (i+1)
        | "STAND" -> printfn "Spiller %i stands" (i+1)
                     stand <- true
                     printfn ""
        | _ -> printfn "Ugyldigt input, prøv med: HIT eller STAND.\n"

// AIs tur
printfn "\n AIs tur"
for i = 0 to AIarray.Length-1 do
    // AI strategi: hit indtil værdien af korthånden er 17 eller over
    while AIarray.[i].Handvalue() < 17 do
        AIarray.[i].Hit()
//Printer AIs hænder
for i = 0 to AIarray.Length-1 do
    printf "AI nummer %As hånd:" (i+1)
    for j = 0 to AIarray.[i].Hand().Length-1 do
        printf "%A, værdi: %A \t" (AIarray.[i].Hand().[j].Name) (AIarray.[i].Hand().[j].Value)
    printfn "\n"

//Dealers tur
printfn "Dealers tur"
// Dealer strategi: ligesom AI
while dealer.Handvalue() < 17 do
    dealer.Hit()
//Printer dealers hånd
printfn "Dealers hånd:"
for k = 0 to dealer.Hand().Length-1 do
    printf "%A, værdi: %A \t" (dealer.Hand().[k].Name) (dealer.Hand().[k].Value)
printfn ""

if dealer.Handvalue() > 21 then
    printfn "Dealer er BUST \n"

// Players: Hvem har vundet? afgøres ud fra kriterier givet i opg.beskrivelsen
printfn "--------------------------"
for i = 0 to PlayerNumber-1 do
    if playerarray.[i].Handvalue() < 22 && dealer.Handvalue() < 22 && playerarray.[i].Handvalue() > dealer.Handvalue() || dealer.Handvalue() > 21 && playerarray.[i].Handvalue() < 22 then
        printfn "Spiller %i har vundet! :-)" (i+1)
    else 
        printfn "Spiller %i har tabt! :-(" (i+1)
// AIs: Hvem har vundet? afgøres ud fra kriterier givet i opg.beskrivelsen
for i = 0 to AINumber-1 do
    if AIarray.[i].Handvalue() < 22 && dealer.Handvalue() < 22 && AIarray.[i].Handvalue() > dealer.Handvalue() || AIarray.[i].Handvalue() < 22 && dealer.Handvalue() > 21  then
        printfn "AI %i har vundet! :-)" (i+1)
    else 
        printfn "AI %i har tabt! :-(" (i+1)
