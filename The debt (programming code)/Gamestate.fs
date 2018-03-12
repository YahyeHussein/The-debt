module Gamestate
open Error
open Helper
open Location
open System

// Group
type Commands = 
    | Inventory
    | Take of string
    | Drop of string
    | Examine of string
    | Help
    | Look 
    | Hint
    | Use of string
    | Turn of string * string
    | Enter of string
    | Write of string
    | Quit

// Group constraint
// Player record type contains the player location, inventory and SpecialPuzzleAnswer
type Player = 
    {
        Location : LocationId
        Inventory : Set<string>
        SpecialPuzzleAnswer : Map<string,string> option
    }

// Group constraint
// The world record type contains the world locations in a map collection type. Also the player label keeps a record of the current player
type World = 
    {
         Locations : Map<LocationId, Location>
         Player : Player
    }

// Group
// It keeps the state of the world using union case
type UpdateGameState =
    | CurrentState of Commands
    | UpdateLocation of (World -> Result<World, string>)
    | EndGameLoop

// Group
// The defualt player 
let player = { 
    Inventory = Set.empty
    Location = LocationId "Bedroom" 
    SpecialPuzzleAnswer = Some(Map.empty
                                        .Add("red valve", "")
                                        .Add("blue valve", "")
                                        .Add("green valve", "")
                              )
}

// Group
// The defualt world with all the locations in a map collection and the current player
let world = { 
    Locations = 
        Map.empty
              .Add(LocationId "Bedroom", bdrst)

              .Add(LocationId "Hallway", lev2hw)
              .Add(LocationId "Hallway2", lev2hw2)
              .Add(LocationId "Hallwaydoor1", lev2hwdr1)
              .Add(LocationId "Stairwell1", strwl1)
              .Add(LocationId "Stairwell2", strwl2)
              .Add(LocationId "Hallwaydoor2", lev2hwdr2)
              
              .Add(LocationId "Attic", attic)
              
              .Add(LocationId "Level 1 Door 3", lev1mrdr3)
              .Add(LocationId "Level 1 Door 2", lev1dr2)
              .Add(LocationId "Massive Room", lev1mrdr)
              .Add(LocationId "Stairwell1", strwl1)

              .Add(LocationId "Grand Floor Hallway", gfhw)
              .Add(LocationId "bag near car", outsidebg)      
              .Add(LocationId "Exit", Exitgame)
              .Add(LocationId "outside", outside)
 
    Player = player
}

// Tai 
// Sets the player location to the locationId
let setCurrentPlace world location =
    { world with Player = { world.Player with Location = location.Id} }

// Tai
// Updates the player location and inventory by using the union to compound the new item.
let updatePlayer world locationId item answer = 
    { world.Player with Location = locationId; Inventory = world.Player.Inventory |> Set.union (Set.empty.Add(item)); SpecialPuzzleAnswer = answer }

// Yahye
// update the player SpecialPuzzleAnswer
let updatePlayerPuzzle world answer = 
    { world.Player with SpecialPuzzleAnswer = answer }

// Tai and Yahye
// sets the player to new player
let setWorldPlayer world player =
    { world with Player = player }

// Yahye
// set a new to the inventory
let setPlayerInventory world item location =
    { world with Locations = location; Player = { world.Player with Inventory = world.Player.Inventory |> Set.union (Set.empty.Add(item)) } }

// Yahye
// It removes the item from inventory
let removeItemFromInventory world item location =
    { world with Locations = location; Player = { world.Player with Inventory = world.Player.Inventory |> Set.remove item } }

// Yahye
// removes the item from the locatin
let removeItemFromLocation location item =
    { location with Items = location.Items |> Set.remove item }

// Yahye
// Sets the item to the location
let setItemInLocation location item = 
    { location with Items = location.Items |> Set.union (Set.empty.Add(item)) }

// shakar 
// Changes the location visited to true
let setVisitedLocation location =
    { location with Visited = true}

// shakar update to new world
// Sets update the locations with a new location map collection
let updateWorldLocations world locations =
    { world with Locations = locations }

// Yahye
// updates the location by getting the location map collection that contains the locationId and location
let updateLocations world changedLocation = world.Locations.Add(world.Player.Location, changedLocation)

// shakar
let describeWorld (location : Location) =
    sprintf "%s--=( %s )=--%sFloor: %i\n\n%s\n\n" (String.replicate 5 " ") (location.Details.Name) (String.replicate 20 " ") (location.Floor) (location.Details.Description)

// Tai 
// It gets the location and matches it to a to the locationId and returns option type
let getLocation world locationId =
    match world.Locations.TryFind locationId with
    | Some location -> Success location
    | None -> Failure "ERROR: Room does not exist!\n"

// Shakar 
// Gets the world player locationId 
// LocationId is used to find the location field that contains the Map<locationId, location> in the world record both records
let getCurrentLocation world =
    world.Player.Location
    |> getLocation world

// Tai 
// The it gets the player location from the world and pipes it to the gertlocation. Also binds the world to describe function that outputs location description
let describeCurrentLocation world =
    world.Player.Location
    |> getLocation world
    |> bind (switch describeWorld)

// Yahye
// It checks if the player has a unquie item that 
let useItem world msg item locationId =
    printfn "%A" world.Player.Inventory
    match world.Player.Inventory.Contains(item) with
    | true ->
        printfn "\nYou have used %s to open the door\n" item
        Success locationId
    | false -> 
        Failure "ERROR: You don't have that item in your inventory\n"

// Tai 
// It gets the direction and exits to check for union cases that allow the user to move around the world
let getLocationExit world direction exits =
    match (direction exits) with
    | AllowedExit (roomId) -> Success roomId
    (*Yahye*)| LockedExit (msg, item, locationId) -> useItem world msg item locationId 
    | NoExit (_) -> Failure "ERROR: There is no room in that direction.\n" 

// Tai 
// The move function pipes the world to to get the location and find room exits 
// The exit is piped to through the getlocation to get the location of the exit.
// It sets the player the a new locationId
let move direction world =
    world
    |> getCurrentLocation
    $ switch (fun room -> room.Exits) 
    $ getLocationExit world direction
    $ getLocation world
    $ switch (setCurrentPlace world)

// Yahye
// The take function is responsilbe for allowing the player to take items from the location and store it to their inventory
let take item world = 
    match world |> getCurrentLocation with
    | Success location -> 
        match location.Items.Contains item with
        | true -> 
            printfn "\nTaken %s {%s}\n" item location.Details.Name
            removeItemFromLocation location item 
            |> updateLocations world
            |> setPlayerInventory world item
        | false -> 
            printfn "\n\nERROR: This item does not exist in this location\n"
            world
    | Failure message -> 
        printfn "\n\n%s\n" message
        world

// shakar 
// The examime function allows the player to check item description
let examine item world =
    match world |> getCurrentLocation with
    | Success location ->
        match location.ItemsDescription with 
        | Some description ->
            
            match description.Item(item) with
            | description -> 
                description |> printfn "%s"
                world
            | _ -> 
                printfn "\n\nERROR: This item does not exist in this location\n" 
                world
        | None ->
            printfn "\n\nERROR: This item does not exist in this location\n"
            world
    | Failure message -> 
        printfn "\n\n%s\n" message
        world

// Tai
// the function allows the player to use flashlight to show hidden text
let _use item world =
    match world |> getCurrentLocation with
    
    | Success location ->
        // in the inital plan the use command was meant to be genric so other items could use it.
        if world.Player.Inventory.Contains("flashlight") then // flashlight("flashlight")
   
        
            let lhd = location.HiddenDescription 
        
            match lhd with
            |Some hiddendiscription ->
                                
                    hiddendiscription |> printf "%s"
                    world
            |None ->
                printfn"\n\nThat item can't be used here\n"
                world
        else
            printfn"\n\nError: you dont have that item to use\n"
            world
    | Failure message -> 
        printfn "\n\n%s\n" message
        world
        //


// Yahye
// The drop function removes items from the player inventory and returns the item to current location items  
let drop item world =
    match world.Player.Inventory.Contains item with
    | true ->
        match world |> getCurrentLocation with
        | Success location ->
            printfn "\nDropped %s from inventory\n" item
            setItemInLocation location item 
            |> updateLocations world
            |> removeItemFromInventory world item
        | Failure message ->
            printfn "\n\n%s\n" message
            world
    | false -> 
        printfn "\n\nERROR: This item does not exist in the inventory\n"
        world

// Tai
// The enter function allow the player to enter values in the game world
let enter value world =

    match world.Player.Location with
    | locationId -> 
        match world.Locations.TryFind locationId with
        | Some location -> 
            match location.comPuz with
            | Some (answer) -> 
                if value = answer then 
                    printfn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Puzzle Solved!!!!!!!!!!!!!!!!!!!!!"
                    printfn "%A" world.Player.Location
                    match world.Player.Location with
                    | LocationId "Attic" -> 
                        // Inventory
                        printfn "The blue key is now in your inventory"
                        None |> updatePlayer world (LocationId "Stairwell2") "bluekey"
                        |> setWorldPlayer world
       
                    | LocationId "Exit" -> 
                        None |> updatePlayer world (LocationId "outside") ""
                        |> setWorldPlayer world
                    | _ -> 
                        world

                else 
                    world
            | None -> 
                world
        | None -> 
            world
            ///////////////

// shakar
// It shows hints of the current location
let hint world = 
    match world |> getCurrentLocation with
    | Success location ->
        match location.Hint with 
        | Some hint -> 
            printfn "%s" hint
            world
        | None -> world
    | Failure message ->
        printfn "\n\n%s\n" message
        world
        /////////////////

// Yahye
// The turn function must allow the player to interact with objects
let turn direction _object world =
    
    match world.Player.Location with
    | locationId -> 
        match world.Locations.TryFind locationId with
        | Some location -> 

                match world.Player.Location with
                | LocationId "Level 1 Door 3" -> 
                    if direction = "left" then
                        if _object = "red valve" then

                            match world.Player.SpecialPuzzleAnswer with
                            | Some answerMap -> 
                                let turnState = answerMap.Add("red valve", "left")
                                let answer = "red valve(" + (turnState.Item("red valve")) + "), blue valve(" +  (turnState.Item("blue valve")) + ") and green valve(" + (turnState.Item("green valve")) + ")"   
                                printfn "You have turned the %s" answer

                                if location.comPuz = Some(answer) then 
                                    printfn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Puzzle Solved!!!!!!!!!!!!!!!!!!!!!"
                                    Some(turnState) |> updatePlayer world (LocationId "Stairwell1") "redkey" |> setWorldPlayer world
                                else Some(turnState) |> updatePlayerPuzzle world |> setWorldPlayer world
                            | None -> 
                                printfn "Not working"
                                world

                        elif _object = "blue valve" then
                            match world.Player.SpecialPuzzleAnswer with
                            | Some answerMap -> 
                                let turnState = answerMap.Add("blue valve", "left")
                                let answer = "red valve(" + (turnState.Item("red valve")) + "), blue valve(" +  (turnState.Item("blue valve")) + ") and green valve(" + (turnState.Item("green valve")) + ")"   
                                printfn "You have turned the %s" answer

                                if location.comPuz = Some(answer) then 
                                    printfn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Puzzle Solved!!!!!!!!!!!!!!!!!!!!!"
                                    Some(turnState) |> updatePlayer world (LocationId "Stairwell1") "redkey" |> setWorldPlayer world
                                else Some(turnState) |> updatePlayerPuzzle world |> setWorldPlayer world
                            | None ->  world

                        elif _object = "green valve" then
                            let turnState = world.Player.SpecialPuzzleAnswer.Value.Add("green valve", "left")
                            let answer = "red valve(" + (turnState.Item("red valve")) + "), blue valve(" +  (turnState.Item("blue valve")) + ") and green valve(" + (turnState.Item("green valve")) + ")"   
                            printfn "You have turned the %s" answer

                            if location.comPuz = Some(answer) then 
                                printfn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Puzzle Solved!!!!!!!!!!!!!!!!!!!!!"
                                Some(turnState) |> updatePlayer world (LocationId "Stairwell1") "redkey" |> setWorldPlayer world
                            else Some(turnState) |> updatePlayerPuzzle world |> setWorldPlayer world

                        else 
                            printfn "That object does not exists"
                            world
                    elif direction = "right" then
                        if _object = "red valve" then
                            let turnState = world.Player.SpecialPuzzleAnswer.Value.Add("red valve", "right")
                            let answer = "red valve(" + (turnState.Item("red valve")) + "), blue valve(" +  (turnState.Item("blue valve")) + ") and green valve(" + (turnState.Item("green valve")) + ")"   
                            printfn "You have turned the %s" answer
                            if location.comPuz = Some(answer) then 
                                printfn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Puzzle Solved!!!!!!!!!!!!!!!!!!!!!"
                                Some(turnState) |> updatePlayer world (LocationId "Stairwell1") "redkey" |> setWorldPlayer world
                            else Some(turnState) |> updatePlayerPuzzle world |> setWorldPlayer world
                        elif _object = "blue valve" then
                            let turnState = world.Player.SpecialPuzzleAnswer.Value.Add("blue valve", "right")
                            let answer = "red valve(" + (turnState.Item("red valve")) + "), blue valve(" +  (turnState.Item("blue valve")) + ") and green valve(" + (turnState.Item("green valve")) + ")"   
                            printfn "You have turned the %s" answer
                            if location.comPuz = Some(answer) then 
                                printfn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Puzzle Solved!!!!!!!!!!!!!!!!!!!!!"
                                Some(turnState) |> updatePlayer world (LocationId "Stairwell1") "redkey" |> setWorldPlayer world
                            else Some(turnState) |> updatePlayerPuzzle world |> setWorldPlayer world
                        elif _object = "green valve" then
                            let turnState = world.Player.SpecialPuzzleAnswer.Value.Add("green valve", "right")
                            let answer = "red valve(" + (turnState.Item("red valve")) + "), blue valve(" +  (turnState.Item("blue valve")) + ") and green valve(" + (turnState.Item("green valve")) + ")"   
                            printfn "You have turned the %s" answer

                            if location.comPuz = Some(answer) then
                                printfn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Puzzle Solved!!!!!!!!!!!!!!!!!!!!!" 
                                Some(turnState) |> updatePlayer world (LocationId "Stairwell1") "redkey" |> setWorldPlayer world
                            else Some(turnState) |> updatePlayerPuzzle world |> setWorldPlayer world
                        else 
                            printfn "That object does not exists"
                            world
                     else 
                        printfn "You entered the wrong direction or object for turn command (left or right)"
                        world

                | _ -> 
                    printfn "\nIt seems you cannot use the turn command at the moment\n"
                    world
            | None -> 
                world
        | None -> 
            world


// shakar
let displayOutput result =
    match result with
    | Success s -> printf "%s" s
    | Failure f -> printf "%s" f

// Yahye 
// Applies the current world whenever the player changes direction
let applyLocationUpdate updateLocation world =
    match updateLocation world with
    | Success newWorld ->
        match newWorld |> getCurrentLocation with
        | Success location -> 
            match location.Visited with
            | true ->
                printfn "%s--=( %s )=--\n" (String.replicate 5 " ") (location.Details.Name)
                newWorld
            | false -> 
                describeCurrentLocation newWorld |> displayOutput
                setVisitedLocation location
                |> updateLocations newWorld
                |> updateWorldLocations newWorld
        | Failure message ->
            printfn "\n\n%s\n" message
            newWorld
    | Failure message ->
        printfn "\n\n%s\n" message
        world

// Group 
// Applies the current world state whenever the user change the state of the world with any of the commands
let applyCommands cmd worldState =
    match cmd with
    // Yahye
    | Inventory -> 
        printfn "\n--=( Inventory )=--\n"
        Set.iter (fun x -> printf " - %s\n"  x) (worldState.Player.Inventory)
        printfn ""
        worldState
    // Yahye
    | Take item -> worldState |> take item
    // shakar
    | Examine item -> worldState |> examine item
    // Yahye
    | Drop item -> worldState |> drop item
    | Help -> worldState
    // Shakar 
    | Look -> 
        describeCurrentLocation worldState |> displayOutput
        worldState 
    // Shakar
    | Hint -> worldState |> hint
    // Yahye
    | Turn (direction, _object) -> worldState |> turn direction _object
     // Tai
    | Enter value -> worldState |> enter value 
     // Tai
    | Use item -> worldState |> _use item
    // group
    | Write value -> worldState
    // Group
    | Quit -> worldState

