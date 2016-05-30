namespace MarsRoverWithoutConditions

module MarsRover =
    type Position = { X: int; Y: int; Direction: Direction }
    and Direction =
        abstract member pivotLeft : unit -> Direction
        abstract member pivotRight : unit -> Direction
        abstract member move : Position -> Position
            
    type N () =
        interface Direction with
            member this.pivotLeft () =
                W() :> Direction
            member this.pivotRight () =
                E() :> Direction
            member this.move p =
                { p with Y = p.Y + 1 }
        override this.Equals otherDirection =
            this.GetType() = otherDirection.GetType()
        override this.GetHashCode () = 1
    and W () =
        interface Direction with
            member this.pivotLeft () =
                S() :> Direction
            member this.pivotRight () =
                N() :> Direction
            member this.move p =
                { p with X = p.X - 1 }
        override this.Equals otherDirection =
            this.GetType() = otherDirection.GetType()
        override this.GetHashCode () = 2
    and S () =
        interface Direction with
            member this.pivotLeft () =
                E() :> Direction
            member this.pivotRight () =
                W() :> Direction
            member this.move p =
                { p with Y = p.Y - 1 }
        override this.Equals otherDirection =
            this.GetType() = otherDirection.GetType()
        override this.GetHashCode () = 3
    and E () =
        interface Direction with
            member this.pivotLeft () =
                N() :> Direction
            member this.pivotRight () =
                S() :> Direction
            member this.move p =
                { p with X = p.X + 1 }
        override this.Equals otherDirection =
            this.GetType() = otherDirection.GetType()
        override this.GetHashCode () = 4


    let W = W () :> Direction
    let E = E () :> Direction
    let S = S () :> Direction
    let N = N () :> Direction
        
    let pivotLeft p = 
        { p with Direction = p.Direction.pivotLeft() }

    let pivotRight p =
        { p with Direction = p.Direction.pivotRight() }

    let move p = p.Direction.move p
        
    let moveBackward p =
        let m = move p
        let deltaX = m.X - p.X
        let deltaY = m.Y - p.Y
        { p with X = p.X - deltaX; Y = p.Y - deltaY }

    let moveNbSteps n p =
        [1..n] |> List.fold (fun p y -> move p) p

    type Instruction() =
        member x.Yield (item:'a) = item

        [<CustomOperation("move")>]
        member x.Move p =
            move

        [<CustomOperation("left")>]
        member x.Left p =
            pivotLeft
            
        [<CustomOperation("right")>]
        member x.Right p =
            pivotRight
    let instruction = Instruction()

module Tests =
    open Xunit
    open FsUnit.Xunit
    open MarsRover

    [<Fact>]
    let ``Given Rover at (0, 0, N), when pivot left, direction is changed to W`` () =
        { X = 0; Y = 0; Direction = N }
        |> pivotLeft
        |> should equal { X = 0; Y = 0; Direction = W }
    
    [<Fact>]
    let ``Given Rover at W, when pivot left, direction is changed to S`` () =
        { X = 4; Y = 5; Direction = W }
        |> pivotLeft
        |> should equal { X = 4; Y = 5; Direction = S }
        
    [<Fact>]
    let ``Given Rover at N, when pivot right, direction is changed to E`` () =
        { X = 4; Y = 5; Direction = N }
        |> pivotRight
        |> should equal { X = 4; Y = 5; Direction = E }
        
    [<Fact>]
    let ``Given Rover at E, when pivot right, direction is changed to S`` () =
        { X = 4; Y = 5; Direction = E }
        |> pivotRight
        |> should equal { X = 4; Y = 5; Direction = S }
        
    [<Fact>]
    let ``Given Rover at (1, 1, N), when move, then new position is (1, 2)`` () =
        { X = 1; Y = 1; Direction = N }
        |> move
        |> should equal { X = 1; Y = 2; Direction = N } 
        
    [<Fact>]
    let ``Given Rover at (1, 1, S), when move, then new position is (1, 0)`` () =
        { X = 1; Y = 1; Direction = S }
        |> move
        |> should equal { X = 1; Y = 0; Direction = S } 
        
    [<Fact>]
    let ``Given Rover at (1, 1, E), when move, then new position is (2, 1)`` () =
        { X = 1; Y = 1; Direction = E }
        |> move
        |> should equal { X = 2; Y = 1; Direction = E } 
        
    [<Fact>]
    let ``Given Rover at (1, 1, W), when move, then new position is (0, 1)`` () =
        { X = 1; Y = 1; Direction = W }
        |> move
        |> should equal { X = 0; Y = 1; Direction = W } 

    [<Fact>]
    let ``Given Rover at (1, 1, N), when move, then new position is (1, 0)`` () =
        { X = 1; Y = 1; Direction = N }
        |> moveBackward
        |> should equal { X = 1; Y = 0; Direction = N } 
        
    [<Fact>]
    let ``Given Rover at (1, 1, S), when move, then new position is (1, 2)`` () =
        { X = 1; Y = 1; Direction = S }
        |> moveBackward
        |> should equal { X = 1; Y = 2; Direction = S } 
        
    [<Fact>]
    let ``Given Rover at (1, 1, E), when move, then new position is (0, 1)`` () =
        { X = 1; Y = 1; Direction = E }
        |> moveBackward
        |> should equal { X = 0; Y = 1; Direction = E } 
        
    [<Fact>]
    let ``Given Rover at (1, 1, W), when move, then new position is (2, 1)`` () =
        { X = 1; Y = 1; Direction = W }
        |> moveBackward
        |> should equal { X = 2; Y = 1; Direction = W }
        
    [<Fact>]
    let ``Given Rover at (1, 1, E), when move 2, then new position is (3, 1)`` () =
        { X = 1; Y = 1; Direction = E }
        |> moveNbSteps 2
        |> should equal { X = 3; Y = 1; Direction = E }

    [<Fact>]
    let ``instruction`` () =
        (instruction {
            move
            left
            move
            move
            right
        }) { X = 0; Y = 0; Direction = N }
        |> should equal { X = -2; Y = 1; Direction = N }
