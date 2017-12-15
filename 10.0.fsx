type Counter (lolxd : int) =
    let mutable xdfunnimeme = lolxd
    member this.value with get () = xdfunnimeme
    member this.incr () = xdfunnimeme <- xdfunnimeme + 1

let sjovmemehaha = new Counter (0)
let sjovmemehaha2 = new Counter(1)
for i = 0 to 49 do
    sjovmemehaha.incr()
printfn "Counter : %A" (sjovmemehaha.value) 