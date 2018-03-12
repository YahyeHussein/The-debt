open System
open Commands
open Gamestate
//open FsControl
//open FSharpPlus


// Group
//reads the user input
let input () = 
    Console.ReadLine () |> fun s -> s.ToLower().Trim()

// Group
//recursive that takes in the input as an argument for the parseinput
let rec parser () = 
    parseInput (input())
    parser () 

//let x = Choice1Of2 []
//(a -> M a >=> a ->  M b)
//
//a -> M b

// parse: string -> Choice<Command, Error>

// executeCommand : State -> Command -> Choice<State * string, Error>

(*
let readInput () = Console.ReadLine()

let handleResult choiceValue =
    match choiceValue  with
    |Choice1Of2 (newstate, outputMsg) ->
        printfn "%s" outputMsg 
        newstate
    |Choice2Of2 error ->
            printoutError error
            currentState  

*)

// let pipeline currentState = 
//          readInput () |> parse >=> executeCommand currentState |> handleResult
        
(*
    
    
     
    | NorthMatch -> gameLoop.Post(UpdateLocation(move north)) 
    
   applyCommands | Drop item -> worldState |> drop item

   *)


[<EntryPoint>]
let main argv = 
    Console.ForegroundColor <- ConsoleColor.White
    Console.Title <- "Debt Text Adventure"
    printfn "Debt Text Adventure"
    printfn "To know what commands you have available, input h or help"
    Console.ForegroundColor <- ConsoleColor.Cyan
    describeCurrentLocation world |> displayOutput
    printf "Input Command -> "
    //calls the parser function
    parser ()
    0 // return an integer exit code