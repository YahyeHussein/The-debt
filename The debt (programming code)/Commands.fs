module Commands
open System
open System.Text.RegularExpressions
open Gamestate
open Gameloop
open Location

let help () =
    
    printfn "                    ->This is the help section<-
                
                        ->Basic Commands<- and ->Action Commands<-

                Take <item>: This command allows you the to take items, which are 
                      available in the game. For example, take (enter item to be taken).
    
                Drop <item>: This command allows you to drop an item from your inventory. 
                      For example, drop (item).

                North or n: This command allows you to move forwards in the game. 
    
                West or w: This command allows you to turn left to see whatever is there.

                East or e: This command allows you to turn right to see whatever is there.
    
                South or s: This command allows you to move backwards in the game.    

                Help or h: Whenever you need to find out what to input, enter this command.

                Hint: This command may help you in a riddle...
    
                Enter or en <value>: This command allows you to enter a room. 
    
                Inventory or inven: This command allows you to see whatever is in your inventory.
    
                Look: This command allows you to look at where you are. 
    
                turn <direction> for <object>: This command allows you to turn things in the game.
    
                Up: This command allows you to go up stairs.
    
                Down: This command allows you to go down stairs.

                Examine or exam <item>: Allows you to examine an item.
            
                Quit: This command allows you to quit the game at any point if you give up."
//a unit the returns unit it terminates the system and returns the exit code.
let quit () =
    printfn "Gave up all ready? Ha Ha Ha. What you got scared or something?"
    Environment.Exit(0)


//pattern  the the user input
let takePattern = "^t(ake){0,1} (\w+)$|^t(ake){0,1} (\w+\s\w+)"
let westPattern = "^w(est){0,1}$"
let writePattern = "^write{0,1} (\w+)$"
let enterPattern = "^en(ter){0,1} (\w+)$"
let turnPattern = "^(turn{0,1}) (\w+) for (\w+\s\w+)$"
let hintPattern = "^hint$"
let eastPattern = "^e(ast){0,1}$"
let southPattern = "^s(outh){0,1}$"
let northPattern = "^n(orth){0,1}$"
let upPattern = "^up$"
let usePattern = "^use (\w+)$"
let downPattern = "^down$"
let helpPattern = "^h(elp){0,1}$|^h$"
let examinePattern = "^exam(ine){0,1} (\w+)$|^exam(ine){0,1} (\w+\s\w+)"
let inventoryPattern = "^inven(tory){0,1}$"
let lookPattern = "^look$"
let dropPattern = "^d(rop){0,1} (\w+)$|^d(rop){0,1} (\w+\s\w+)"
let quitPattern = "^quit$"

//matches the pattern of the user input
let (|TakeMatch|NorthMatch|EastMatch|WestMatch|SouthMatch|NoMatch|) input =
    match Regex.Match(input, takePattern), Regex.Match(input, northPattern),
          Regex.Match(input, eastPattern), Regex.Match(input, westPattern), Regex.Match(input, southPattern) with
    |takeMatch, northMatch, eastMatch, westMatch, southMatch when takeMatch.Success ->
        TakeMatch takeMatch.Groups.[2].Value
    |takeMatch, northMatch, eastMatch, westMatch, southMatch when northMatch.Success ->
        NorthMatch
    |takeMatch, northMatch, eastMatch, westMatch, southMatch when eastMatch.Success ->
        EastMatch
    |takeMatch, northMatch, eastMatch, westMatch, southMatch when westMatch.Success ->
        WestMatch
    |takeMatch, northMatch, eastMatch, westMatch, southMatch when southMatch.Success ->
        SouthMatch
    |_ ->
        NoMatch 
    
let (|UpMatch|DownMatch|ExamineMatch|WriteMatch|EnterMatch|TurnMatch|NoMatch|) input = 
    match Regex.Match(input, upPattern), Regex.Match(input, downPattern), Regex.Match(input, examinePattern), 
          Regex.Match(input, writePattern), Regex.Match(input, enterPattern), Regex.Match(input, turnPattern) with
    |upMatch, downMatch, examineMatch, writeMatch, enterMatch, turnMatch when upMatch.Success ->
        UpMatch
    |upMatch, downMatch, examineMatch, writeMatch, enterMatch, turnMatch when downMatch.Success ->
        DownMatch
    |upMatch, downMatch, examineMatch, writeMatch, enterMatch, turnMatch when examineMatch.Success ->
        ExamineMatch examineMatch.Groups.[2].Value
    |upMatch, downMatch, examineMatch, writeMatch, enterMatch, turnMatch when writeMatch.Success ->
        WriteMatch writeMatch.Groups.[2].Value
    |upMatch, downMatch, examineMatch, writeMatch, enterMatch, turnMatch when enterMatch.Success ->
        EnterMatch enterMatch.Groups.[2].Value
    |upMatch, downMatch, examineMatch, writeMatch, enterMatch, turnMatch when turnMatch.Success ->
        TurnMatch (turnMatch.Groups.[2].Value, turnMatch.Groups.[3].Value)
    |_ ->
        NoMatch

//group
let (|HelpMatch|InventoryMatch|LookMatch|DropMatch|QuitMatch|HintMatch|NoMatch|) input =
    match Regex.Match(input, helpPattern), Regex.Match(input, inventoryPattern),
          Regex.Match(input, lookPattern), Regex.Match(input, dropPattern), Regex.Match(input, quitPattern), 
          Regex.Match(input, hintPattern) with
    |helpMatch, inventoryMatch, lookMatch, dropMatch, quitMatch, hintMatch when helpMatch.Success ->
        HelpMatch
    |helpMatch, inventoryMatch, lookMatch, dropMatch, quitMatch, hintMatch when inventoryMatch.Success ->
        InventoryMatch
    |helpMatch, inventoryMatch, lookMatch, dropMatch, quitMatch, hintMatch when lookMatch.Success ->
        LookMatch
    |helpMatch, inventoryMatch, lookMatch, dropMatch, quitMatch, hintMatch when dropMatch.Success ->
        DropMatch dropMatch.Groups.[2].Value
    |helpMatch, inventoryMatch, lookMatch, dropMatch, quitMatch, hintMatch when quitMatch.Success ->
        QuitMatch
    |helpMatch, inventoryMatch, lookMatch, dropMatch, quitMatch, hintMatch when hintMatch.Success ->
        HintMatch
    |_ ->
        NoMatch

let (|UseMatch|NoMatch|) input =
    match Regex.Match(input, usePattern) with
    |useMatch when useMatch.Success ->
       UseMatch useMatch.Groups.[2].Value
    |_ ->
       NoMatch

// Features
////// Look (Yes)
////// Take (Yes)
////// Drop (Yes)
////// Inventory (Yes)
////// Examine (Yes) 

// Group 
//uses  patern matching to match the user input to the active pattern match
let parseInput input =
    match input with
    // Tai
    | NorthMatch -> gameLoop.Post(UpdateLocation(move north)) 
    | EastMatch -> gameLoop.Post(UpdateLocation(move east)) 
    | WestMatch -> gameLoop.Post(UpdateLocation(move west)) 
    | SouthMatch -> gameLoop.Post(UpdateLocation(move south))
    | DownMatch -> gameLoop.Post(UpdateLocation(move down))
    | UpMatch -> gameLoop.Post(UpdateLocation(move up))
    // Shakar
    | LookMatch -> gameLoop.Post(CurrentState Look)
    // Yahye
    | TakeMatch item -> gameLoop.Post(CurrentState (Take(item)))
    // Yahye
    | InventoryMatch -> gameLoop.Post(CurrentState (Inventory))
    // Yahye
    | DropMatch item -> gameLoop.Post(CurrentState (Drop(item)))
    // Shakar
    | ExamineMatch item -> gameLoop.Post(CurrentState (Examine(item)))
    // Shakar
    | HintMatch -> gameLoop.Post(CurrentState (Hint))
    // Yahye
    | TurnMatch (direction, _object) -> gameLoop.Post(CurrentState (Turn(direction, _object)))
    // Tai
    | EnterMatch value -> gameLoop.Post(CurrentState (Enter(value)))
    // Tai
    | UseMatch item -> gameLoop.Post(CurrentState (Use(item)))
    //Group
    | WriteMatch value -> gameLoop.Post(CurrentState (Write(value)))
    // Group
    | QuitMatch -> quit()
    // Shakar
    | HelpMatch -> 
        help()
        printf "Input Command -> "
    | NoMatch -> 
        printfn "Wrong command or requires agrements for help enter the (help) command"
        printf "Input Command -> "
    |_->()

    //whatever happens when time of death takes place this will show up
let gameOver () = 
    printfn "================="
    printfn "|| You died!!! ||"
    printfn "================="
    gameLoop.Post(EndGameLoop)
    gameLoop.Start()