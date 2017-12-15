type Drone (pos : float * float, dest : float * float, hast : float) =
    member this.position() = pos
    member this.speed() = hast
    member this.destination() = dest
    member this.fly() = 