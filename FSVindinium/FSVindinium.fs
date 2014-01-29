module FSVindinium

open FSharp.Data
open FSharp.Net

type Parser = JsonProvider<"Sample.json">

// 'Stay', 'North', 'South', 'East', 'West'
type Moves =
    | Stay
    | North
    | South
    | East
    | West
with 
    override t.ToString() = 
        match t with
        | Stay -> "stay" | North -> "north" | South -> "south" | East -> "east" | West -> "west"
    member t.Reverse = 
        match t with
        | Stay -> Stay | North -> South | South -> North | East -> West | West -> East
    static member FromString (str: string) = 
        match str with
        | "stay" -> Stay | "north" -> North | "south" -> South | "east" -> East | "west" -> West
        | s -> failwithf "Failed to convert %s, it is not a valid move" s

let private makeWebRequest url queryVals =
    let result = Http.Request(url = url, query = queryVals, meth = "POST")
    
    // 200: Everything went well, good job!
    // 4xx (400, 404, …): You did something wrong (wrong secret key, trying to play when the game is already finished, too slow to send the move, …). 
    //                    Be sure to check the response body to know what the exact error is.
    // 500: Something went wrong on the server side. How could it be possible? ;)

    match result.StatusCode, result.Body with
    | 200, ResponseBody.Text txt -> Parser.Parse(txt)
    | rc, _ when rc >= 400 && rc < 500 -> failwithf "Http Response code: %i, You did something wrong (wrong secret key, trying to play when the game is already finished, too slow to send the move, …)" rc
    | 500, _ -> failwith "Something went wrong on the server side. How could it be possible? ;)"
    | _ -> failwithf "Unexpected type in http response body."          

let private makeMove (key: string) (url: string) (move: Moves) =
    // key (required)
    //      The secret key you wrote down after registering at the register page
    // dir (required)
    //      Can be one of: 'Stay', 'North', 'South', 'East', 'West' 
    let dir = match move with | Stay -> "stay" | North -> "north" | South -> "south" | East -> "east" | West -> "west"
    let queryVals = ["key", key; "dir", dir]
    makeWebRequest url queryVals

type VindiniumGame(key: string, state: Parser.Entity) = 
    member t.StartingState = state
    member t.Move(move: Moves) = makeMove key state.PlayUrl move

/// Turns - The number of turns you want to play. If you don't specify this parameter, 300 turns will be played.
/// Map - The map id corresponding to the map you want to use. Possible values are: m1, m2, m3, m4, m5, m6. 
let startTraining (key: string) (turns: int option) (map: string option) =
    // key (required)
    //      The secret key you wrote down after registering at the register page
    // turns
    //      The number of turns you want to play. If you don't specify this parameter, 300 turns will be played.
    // map
    //      The map id corresponding to the map you want to use. Possible values are: m1, m2, m3, m4, m5, m6. 
    //      You can have a look at the maps here. If you don't specify this parameter, a random map will be generated for you. 
    let queryVals = 
        [
            yield "key", key
            yield! turns |> Option.map (fun turns -> "turns", string turns) |> Option.toList
            yield! map   |> Option.map (fun map   -> "map"  , map)          |> Option.toList
        ]
    let entity = makeWebRequest @"http://vindinium.org/api/training" queryVals
    VindiniumGame(key, entity)

let startArena (key: string) =
    //key (required)
    //     The secret key you wrote down after registering at the register page 
    let queryVals = [ "key", key ]
    let entity = makeWebRequest @"http://vindinium.org/api/arena" queryVals
    VindiniumGame(key, entity)


