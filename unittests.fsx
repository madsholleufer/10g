(* Unit tests:
- Metoderne i Card klassen
- CardDraw()
- Randomizer()
- PlayerNumber pattern matching (l. 115)
- AINumber pattern matching (l. 129)
- Metoderne i Player klassen
	- Hit() funktionalitet
	- Stand() funktionalitet
	- hvad sker der ved forkert input?
- Metoderne i Dealer klassen
- AI strategi ( = dealer strategi)
- Vinder folk rigtigt?
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
let mutable deck = [|card1;card2;card4;card5|]

// Test af metoderne i Card klassen
printfn "name=%A, val=%A" card1.Name card1.Value
printfn "name=%A, val=%A" card2.Name card2.Value
printfn "name=%A, val=%A" card3.Name card3.Value
card3.SetValue(11)
printfn "new value for card3=%A" card3.Value


(* Vi antager, at koden der genererer et tilfældigt tal (fra 
	opgavebeskrivelsen) virker. *)
let gen = System.Random()

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

// Test af CardDraw(x) og Randomizer(): fjernes kortene fra dækket?
for i = 0 to 3 do
    let res = (Randomizer())
    printfn "%A" deck.Length

// Test af brugerinput til antallet af spillere (PlayerNumber)
printfn "Hvor mange spillere? "
let PlayerNumber : int array = [|2;-1;7|]
for i = 0 to PlayerNumber.Length-1 do
    try // Forventer at der håndteres den exception, der kastes (fejlbeskeden printes)
        match PlayerNumber.[i] with
        | x when x < 1 -> failwith "For få spillere"
        | x when x > 5 -> failwith "For mange spillere"
        | _ -> printfn "OK."
    with
    | Failure msg -> printfn "%s" msg
//Samme kode for antallet af AI, så det undlades i vores tests.

// Test af metoderne i Player klassen
