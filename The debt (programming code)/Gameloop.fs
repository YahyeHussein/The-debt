module Gameloop
open Gamestate


// Yahye
let gameLoop =


    MailboxProcessor.Start(fun inbox ->
        let rec worldLoop worldState = // This has the current world
            async {
                printf "Input Command -> "
                
                let! msg = inbox.Receive()
               
                match msg with
                | UpdateLocation updateLocation -> return! worldLoop (applyLocationUpdate updateLocation worldState)
                | CurrentState cmd -> return! worldLoop (applyCommands cmd worldState)
                | EndGameLoop -> return ()
            }
           

        // This kickstart the game with the defualt world state    
        worldLoop world
    )
