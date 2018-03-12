module Location

// All group

// Group constraint
type Details =
    { 
        Name: string
        Description: string 
    }

// Group constraint
type Item =
    {
        items : Set<string>
        ItemsDescription : Map<string,string>
        ItemsSentence : Map<string,string>
    }

// Group constraint
//this is value that takes in a string
type LocationId =
    | LocationId of string

type Exit =
    | AllowedExit of LocationId
    | LockedExit of description: string * key: string * next: LocationId
    | NoExit of string option
    //this is value that takes in a string
type Exits = 
    { 
        West : Exit
        East : Exit 
        South : Exit
        North : Exit
        Up : Exit
        Down : Exit
    }

// Group constraint
type Location = 
    {
        Id : LocationId
        Details : Details
        Items : Set<string>
        ItemsDescription : Map<string,string> option
        HiddenDescription : string option
        Hint : string option
        Exits : Exits
        Floor : int
        Visited : bool
        gameOver : bool
        comPuz : string option // completed puzzle answer
        // Solvedpuz : bool
    }


    //these are the records that contain details about the location.
let bdrst = { 
    Id = LocationId "Bedroom"
    Details = {Name = "You are in the Bedroom"; Description = "\nThere is a door to the north\nThere is bag here"}
    Items = Set.empty.Add("bag").Add("redkey")
    ItemsDescription = Some(Map.empty.Add("bag", "It's just a normal looking bag."))
    HiddenDescription = Some ("The light has reavealed a book...\nTitle: 'Sleep deprevation can kill you'")
    Hint = None
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = NoExit(None)
            North = AllowedExit(LocationId "Hallway")
            Up = NoExit(None)
            Down = NoExit(None) }
    Floor = 2
    Visited = true
    gameOver = false
    comPuz = None
}
//////////////////////////////////////////////////////////////////////////////////////

let lev2hw = {
    Id = LocationId "Hallway" 
    Details = {Name = "You are in the Hallway"; Description = "There is a door to your wast and east\nThere is also another to your south" }
    Items = Set.empty.Add("key")
    ItemsDescription = None
    HiddenDescription = Some ("The hallway structure is very old... there are cracks in the ceiling ")
    Hint = None
    Exits = 
        {   West = AllowedExit(LocationId "Hallwaydoor2" )
            East = AllowedExit(LocationId "Hallwaydoor1" )
            South = AllowedExit(LocationId "Bedroom")
            North = AllowedExit(LocationId "Hallway2")
            Up = NoExit(None)
            Down = NoExit(None)}
    Floor = 2
    Visited = false
    gameOver = false
    comPuz = None
}
let lev2hw2 = {
    Id = LocationId "Hallway2" 
    Details = {Name = "You are in the second area of the Hallway"; Description = "There is another door to your west and east\nThere is also another to your north" }
    Items = Set.empty 
    ItemsDescription = None
    HiddenDescription = None
    Hint = None
    Exits = 
        {   West = AllowedExit(LocationId "Hallwaydoor4")
            East = AllowedExit(LocationId "Hallwaydoor3")
            South = AllowedExit(LocationId "Hallway")
            North = AllowedExit(LocationId "Stairwell2")
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 2
    Visited = false
    gameOver = false
    comPuz = None
   
} 
let lev2hwdr1 = {
    Id = LocationId "Hallwaydoor1" 
    Details = {Name = "You are in door 1 of the hallway"; Description = "There is a flashlight in this room \n There might items be more elsewhere...." }
    Items = Set.empty.Add("flashlight")
    ItemsDescription = Some(Map.empty.Add("flashlight", "A battery powered flashlight"))
    HiddenDescription = None
    Hint = None
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = AllowedExit(LocationId "Hallway")
            North = NoExit(None)
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 2
    Visited = false
    gameOver = false
    comPuz = None

} 

let lev2hwdr2 = {
    Id = LocationId "Hallwaydoor2" 
    Details = {Name = "You are in door 2 of the hallway"; Description = "Ah! shit there is a tiger and it just jumped you and then whilst it is eating you\n
    a clown comes and trows pie in your face and you sufficate to death. ha ha ha ha ha ha." }
    Items = Set.empty
    ItemsDescription = None
    HiddenDescription = None
    Hint = None
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = NoExit(None)
            North = NoExit(None)
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 2
    Visited = false
    gameOver = true
    comPuz = None
} 

let lev2hwdr3 = {
    Id = LocationId "Hallwaydoor3" 
    Details = {Name = "You are in door 3 of the hallway"; Description = "There is a batchroom in there.\n You can tell that the bathroom has been used." }
    Items = Set.empty
    ItemsDescription = None
    HiddenDescription = None
    Hint = None
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = AllowedExit(LocationId "Hallway2")
            North = NoExit(None)
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 2
    Visited = false
    gameOver = false
    comPuz = None
} 
let lev2hwdr4 = {
    Id = LocationId "Hallwaydoor4" 
    Details = {Name = "You are in door 4 of the hallway"; Description = "As you have entered the room, the door has slammed shut\n The walls are closing in on you.... " }
    Items = Set.empty
    ItemsDescription = None
    HiddenDescription = None
    Hint = None
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = NoExit(None)
            North = NoExit(None)
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 2
    Visited = false
    gameOver = false
    comPuz = None
   
} 

let strwl2 = {
    Id = LocationId "Stairwell2" 
    Details = {Name = "You are in the level 2 starirwell"; Description = "This stairwell consists of 3 levels\n Each Level can be accessed through the stairwell" }
    Items = Set.empty 
    ItemsDescription = None
    HiddenDescription = None
    Hint = None
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = AllowedExit(LocationId "Hallway2")
            North = NoExit(None)
            Up = AllowedExit(LocationId "Attic")
            Down = LockedExit("The door is locked, it seems it requires a key", "bluekey" , LocationId "Massive Room")  }
    Floor = 2
    Visited = false
    gameOver = false
    comPuz = None
    
}



/////////////////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////

let attic = {
    Id = LocationId "Attic" 
    Details = 
            {
                Name = "You are in the Attic"; 
                Description = "The door suddenly shut.\nThe attic is filled with boxes that may reveal how to get out \nThey seems to be paper on the ground \na strange 4 digit number is painted on the wall with blood" 
            }
    Items = Set.empty.Add("paper")
    ItemsDescription = Some(Map.empty
                        .Add("boxes", "You open the box and see a lot of junk, like computer mouse, monitor and keyboard")
                        .Add("wall", "You see 4 digits taht are 1234")
                        .Add("paper", "I have keys but no locks. I have a space but no room. You can enter, but can’t go outside. What am I?")
                        )
    HiddenDescription = None
    Hint = Some("Look in the boxes")
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = NoExit(None)
            North = NoExit(None)
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 2
    Visited = false
    gameOver = false
    comPuz = Some("keyboard")
}
//////////////////////////////////////////////////////////////////////////////////////////////////////



let lev1mrdr = {
    Id = LocationId "Massive Room" 
    Details = {Name = "You are in a massive room"; Description = "As you've entered level 1, you see that it is a massive room.\n You look around and see 3 doors and a window, what do you want to do first?" }
    Items = Set.empty
    ItemsDescription = None
    HiddenDescription = None
    Hint = None
    Exits = 
        {   West = LockedExit("The door is locked, it seems it requires a key", "redkey" , LocationId "Stairwell1") 
            East = AllowedExit(LocationId "Level 1 Door 3")
            South = AllowedExit(LocationId "Stairwell2")
            North = AllowedExit(LocationId "Level 1 Door 2")
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 1
    Visited = false
    gameOver = false
    comPuz = None
}

let lev1dr2 = {
    Id = LocationId "Level 1 Door 2" 
    Details = {Name = "You are in a massive room near door 2"; Description = "You opened the door and saw a flashing light that blinded\n You can't seem to get out from room.\n GAME OVER!!"}
    Items = Set.empty
    ItemsDescription = None
    HiddenDescription = None
    Hint = None
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = NoExit(None)
            North = NoExit(None)
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 1
    Visited = false
    gameOver = false
    comPuz = None
}

let lev1mrdr3 = {
    Id = LocationId "Level 1 Door 3" 
    Details = {Name = "You are in a massive room near door 2"; Description = "The door you came is locked\nYou see a timer\n The floor is falling up with water\There seems to be three valves with colours (red,blue,green)" }
    Items = Set.empty.Add("redkey")
    ItemsDescription = None
    HiddenDescription = None
    Hint = Some("rL, bR, gL")
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = NoExit(None)
            North =  NoExit(None)
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 2
    Visited = false
    gameOver = false
    comPuz = Some("red valve(left), blue valve(right) and green valve(left)")
}

let strwl1 = {
    Id = LocationId "Stairwell1" 
    Details = {Name = "You are in the level 1 starirwell"; Description = "This stairwell takes you to the grandfloor" }
    Items = Set.empty 
    ItemsDescription = None
    HiddenDescription = None
    Hint = None
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = AllowedExit(LocationId "Massive Room")
            North = NoExit(None)
            Up = NoExit(None)
            Down = AllowedExit(LocationId "Grand Floor Hallway")  }
    Floor = 2
    Visited = false
    gameOver = false
    comPuz = None
}
///////////////////////////////////////////////////////////////////////////////////////////////////////

let outsidebg = {
    Id = LocationId "bag near car" 
    Details = {Name = "You are near the bag near the car"; Description = "As you're approaching the car almost excited to leave this hell of a house. You get a last flashback.\n You were growing up, you were so happy, you went to collage then university and graduated,\n shortly after you found an amazing job, went exploring found love and came home,\n got married and had a beautiful child. You loved that child so much, with all your heart\n and can,t imagine your life without your child.\n you then open the bag.\n It is your beloved\n child" }
    Items = Set.empty
    ItemsDescription = None
    HiddenDescription = Some("The ground is very muddy, there are trees and bushes around the area")
    Hint = None
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = NoExit(None)
            North = NoExit(None)
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 0
    Visited = false
    gameOver = false
    comPuz = None
}

let gfhw = {
    Id = LocationId "Grand Floor Hallway" 
    Details = {Name = "You are in the grand floor hallway"; Description = "You've opened the ground floor door.\nAs your walking through it you see that it is a long hallway, and the exit door is in front of you.\nAs you are approaching the door you see blood on the walls you see that it is written would you like to read it.\nThe wall tells of a story of you life, just before you came here, it reads that you have been successful in your life, not just in education but work. As well as in love. " }
    Items = Set.empty
    ItemsDescription = None
    HiddenDescription = None
    Hint = None
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = AllowedExit(LocationId "Stairwell1")
            North = AllowedExit(LocationId "Exit")
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 0
    Visited = false
    gameOver = false
    comPuz = None
}

let Exitgame = {
    Id = LocationId "Exit" 
    Details = {Name = "You have reached the exit door"; Description = "There is a four digit comboniation lock on the door" }
    Items = Set.empty
    ItemsDescription = None
    HiddenDescription = None
    Hint = Some("Remember the attic")
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = AllowedExit(LocationId"GroundFloor") 
            North =  NoExit(None)
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 2
    Visited = false
    gameOver = false
    comPuz = Some("1234")
    
}

let outside = {
    Id = LocationId "outside" 
    Details = {Name = "You are outside the house"; Description = "You've solved the puzzle, well done.\n Then door opens.\n As you walk outside the door behind slanms shut, you see the car.\n As you are walking towards the car you see a big bag next to it. There is the car\nYou approches the car\nAnd suddenly you get this flash back" }
    Items = Set.empty
    ItemsDescription = None
    HiddenDescription = None
    Hint = None
    Exits = 
        {   West = NoExit(None)
            East = NoExit(None)
            South = NoExit(None) 
            North =  AllowedExit(LocationId "bag near car")
            Up = NoExit(None)
            Down = NoExit(None)  }
    Floor = 2
    Visited = false
    gameOver = false
    comPuz = None
}

//directions that return the locationId
let north ({ North = northExit } : Exits) = northExit
let south ({ South = southExit } : Exits) = southExit
let east ({ East = eastExit } : Exits) = eastExit
let west ({ West = westExit } : Exits) = westExit
let down ({ Down = downExit } : Exits) = downExit
let up ({ Up = upExit } : Exits) = upExit