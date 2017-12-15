type Moth (x:float, y:float) =
    let mutable MothPos = (x,y)
    static let LysPos = (500.,200.)
    member this.moveToLight() = MothPos <- (((fst(MothPos) + fst(LysPos))/2.),((snd(MothPos) + snd(LysPos))/2.))
    member this.GetPosition() = MothPos

let Peter = new Moth(10.,10.)
Peter.moveToLight()
printfn " Postmand Per %A" (Peter.GetPosition())

