
#r @".\packages\FSharp.Data.1.1.10\lib\net40\FSharp.Data.dll"
#r @".\bin\Release\FSVindinium.dll"

open System 
open FSharp.Data
open FSVindinium

// Put your API key here!
// If you don't have one register at: http://vindinium.org
let key = "dbhhqnzq" 

// Helpers
let rnd = new Random()
let getRandomMove () =     
    match rnd.Next(4) with
    | 0 -> Stay
    | 1 -> North
    | 2 -> South                            
    | 3 -> East
    | 4 -> West
    | i -> failwithf "Unexpected Random Number: %i" i


// Define AI
let startAI (game: VindiniumGame) =
    let rec aiLoop (state: Parser.DomainTypes.Entity) = 
        // If finished return the final game state
        if state.Game.Finished then 
            printfn "Finished"
            state                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
        else
            // Otherwise do a random move and give it another go
            let nextMove = getRandomMove()
            printfn "Move for round %i of %i: %s" (state.Game.Turn) (state.Game.MaxTurns) (nextMove.ToString())
            let newState = game.Move (nextMove)
            aiLoop newState
    aiLoop game.StartingState

// Run in Training Mode with Default Params
let trainInstance = startTraining key None None
startAI trainInstance

// Run in Arena Mode
//let arenaInstance = startArena key