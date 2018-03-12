module Error

// All group

type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

// The bind function takes in two functions
// The first function takes in the switch function the changes a regular function to Result<success,failure> 
// The second function is used to match with a success and failure
// The success returns a the switch and takes in a input
// The failure return a failure with a string
let bind processFunc lastResult =
    match lastResult with
    | Success s -> processFunc s
    | Failure f -> Failure f

let switch processFunc input =
    Success (processFunc input)
