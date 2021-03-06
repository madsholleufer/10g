(* Tests:
- Metoderne i Card klassen
- CardDraw() og Randomizer()
- PlayerNumber pattern matching (l. 115)
- AINumber pattern matching (l. 129)
- Metoderne i Player klassen
	- Hit() funktionalitet
- Metoderne i Dealer klassen
*)

//Definerer Card klassen
type Card (cardvalue : int, name : string) =
    let mutable value = cardvalue
    member this.Value = value
    // Gør det muligt at overskrive kortets værdi. Bruges når kortet er et es, hvilket kan være 1 eller 11.
    member this.SetValue(newvalue : int) = value <- newvalue
    member this.Name = name

// Definerer kort til tests
let card1 = new Card(8, "ottehjerte")
let card2 = new Card(10, "knægtklør")
let card3 = new Card(1, "esspar")
let card4 = new Card(10,"damerude")
let card5 = new Card(3,"trerude")

// Test dæk
let mutable deck = [|card1;card2;card3;card4;card5|]

// Test af metoderne i Card klassen
printfn "\nTEST: laves korrekte kort"

let test1 = 
    let exp_val = 8
    let exp_name = "ottehjerte"
    let value = card1.Value
    let name = card1.Name
    printfn "name=%A, val=%A success= %b" exp_name exp_val (exp_val=value && exp_name = name)

let test2 = 
    let exp_val = 10
    let exp_name = "knægtklør"
    let value = card2.Value
    let name = card2.Name
    printfn "name=%A, val=%A success= %b" exp_name exp_val (exp_val=value && exp_name = name)

let test3 = 
    let exp_val = 1
    let exp_name = "esspar"
    let value = card3.Value
    let name = card3.Name 
    printfn "name=%A, val=%A success= %b" exp_name exp_val (exp_val=value && exp_name = name)
    
let test4 = 
    let new_exp = 11
    card3.SetValue(11)
    printfn "new value for card3=%A success=%b" new_exp (card3.Value = new_exp)


//Test af CardDraw() og Randomizer()
let test5 = 
    // Test af CardDraw(x) og Randomizer(): fjernes kortene fra dækket?
    // Dækket har først længden 5. Der fjernes et kort, så har det længden 4 osv.
    
    (* Vi antager, at koden der genererer et tilfældigt tal (fra 
	opgavebeskrivelsen) virker. *)
    let gen = System.Random()

    // Definerer funktionerne
    let CardDraw (x : int) = 
        // trækker et kort vha. indeksering i dæk arrayet.
        let returncard = (deck.[x])
        // Fjerner kortet fra dækket
        deck <- deck |> Array.filter ((<>)deck.[x])
        // returnerer kortet, der blev trukket
        returncard

    let Randomizer() =
        let x = gen.Next(0, deck.Length) // indtil sidste indeks i arrayet
        CardDraw (x)

    //Tests
    printfn "\nTEST: fjernes kort korrekt fra dækket"
    for i = 4 downto 1 do
        let res = Randomizer()
        printfn "Deck length: %A success?=%b" (deck.Length) (deck.Length = i)
    printfn ""

let test6 =
    // Test af CardDraw(x) og Randomizer(): returneres et kort?
    //Tester med et nyt dæk, fordi nu har vi fjernet kort i forrige tests
    let mutable deck2 = [|card1;card2;card3;card4;card5|]

    (* Vi antager, at koden der genererer et tilfældigt tal (fra 
	opgavebeskrivelsen) virker. *)
    let gen = System.Random()

    // Definerer funktionerne
    let CardDraw (x : int) = 
        // trækker et kort vha. indeksering i dæk arrayet.
        let returncard = (deck2.[x])
        // Fjerner kortet fra dækket
        deck2 <- deck2 |> Array.filter ((<>)deck2.[x])
        // returnerer kortet, der blev trukket
        returncard

    let Randomizer() =
        let x = gen.Next(0, deck2.Length) // indtil sidste indeks i arrayet
        CardDraw (x)

    //Tests
    //Den trækker kort tilfældigt så vi kan kun tjekke om kortets værdi 
    //er i det korrekte interval (1 til 11, begge inklusive).
    printfn "TEST: korrekte kortværdier"
    for i = 0 to 4 do
        let res = Randomizer()
        printfn "Kortnavn: %A , kortværdi: %A success?=%b" res.Name res.Value (res.Value <= 11 && res.Value >= 1)
    printfn ""


// Test af brugerinput til antallet af spillere (PlayerNumber)
// Vi må kigge i konsollen og se om det passer (det gør det).
// Test af dette er inkluderet i rapporten i bilagene.
printfn "TEST: antal spillere"
let PlayerNumber : int array = [|2;-1;7|] //Test cases
for i = 0 to PlayerNumber.Length-1 do
    try // Forventer at der håndteres den exception, der kastes (fejlbeskeden printes)
        match PlayerNumber.[i] with
        | x when x < 1 -> failwith "For få spillere"
        | x when x > 5 -> failwith "For mange spillere"
        | _ -> printfn "OK."
    with
    | Failure msg -> printfn "%s" msg
//Samme kode for antallet af AI, så det inkluderes ikke her.


// Test af metoderne i Player klassen

// Nyt test dæk
let mutable deck3 = [|card1;card2;card4;card5|]

//Vi får brug for følgende til en metode i Player klassen:
let gen = System.Random()
// Definerer funktionerne
let CardDraw (x : int) = 
    // trækker et kort vha. indeksering i dæk arrayet.
    let returncard = (deck3.[x])
    // Fjerner kortet fra dækket
    deck3 <- deck3 |> Array.filter ((<>)deck3.[x])
    // returnerer kortet, der blev trukket
    returncard
let Randomizer() =
    let x = gen.Next(0, deck3.Length) // indtil sidste indeks i arrayet
    CardDraw (x)

// Definerer spiller klassen, som har en korthånd, en korthåndværdi samt nogle metoder, der beskrives nedenfor.
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

(* 
* Vi tester metoden Hit() og ser om spilleren rent faktisk får tildelt et kort.
* Vi tester desuden om spillerens korthåndværdi (handvalue) er korrekt.
* Det er svært at teste, når der bruges kode, der genererer et tilfældigt tal.
* Men det ser ud til at virke.
*)
printfn "\nTEST: korthåndværdi og korthånd (Hit() funktionalitet)"
let testPlayer = new Player()
testPlayer.Hit()
let mutable currentHandVal = testPlayer.Hand().[0].Value
printfn "\nKorthåndværdi: %A , success=%b" (testPlayer.Handvalue()) ((testPlayer.Handvalue())=currentHandVal)
testPlayer.Hit()
currentHandVal <- currentHandVal + testPlayer.Hand().[1].Value
printfn "Korthåndværdi: %A, success=%b" (testPlayer.Handvalue()) ((testPlayer.Handvalue())=currentHandVal)
testPlayer.Hit()
currentHandVal <- currentHandVal + testPlayer.Hand().[2].Value
printfn "Korthåndværdi: %A, success=%b" (testPlayer.Handvalue()) ((testPlayer.Handvalue())=currentHandVal)
// Printer korthånd (navne og værdier)
for i = 0 to testPlayer.Hand().Length - 1 do
    printfn "Korthånd: \nnavn= %A , værdi=%A" (testPlayer.Hand().[i].Name) (testPlayer.Hand().[i].Value)
