namespace ScrabbleWabble

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

open Utility

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun t ->
            match t.Value with
            | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
            | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    type coord = int * int

    // Add map to the state that is the state of the board
    type state =
        { board: Parser.board
          dict: ScrabbleUtil.Dictionary.Dict
          playerNumber: uint32
          numberOfPlayers: uint32
          playerTurn: uint32
          hand: MultiSet.MultiSet<uint32>
          placedTiles: Map<coord, (uint32 * (char * int))> 
        }

    let mkState b d pn np plt h pt =
        { board = b
          dict = d
          playerNumber = pn
          numberOfPlayers = np
          playerTurn = plt
          hand = h
          placedTiles = pt 
        }

    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand
    let placedTiles st = st.placedTiles

module Scrabble =
    open System.Threading

    type move = (coord * (uint32 * (char * int)))
    type movesInWordList = move List

    let checkDirection ((x, y) : coord) (direction : coord) (placedTiles : Map<coord, (uint32 * (char * int))>) : bool =
        match direction with
        | (1,0) -> // Moving right
            match Map.tryFind (x+1, y) placedTiles with 
            | None when Map.tryFind (x, y+1) placedTiles = None && Map.tryFind (x, y-1) placedTiles = None -> true
            | _ -> false
        | (0,1) -> // Moving down
            match Map.tryFind (x, y+1) placedTiles with
            | None when Map.tryFind (x+1, y) placedTiles = None && Map.tryFind (x-1, y) placedTiles = None -> true
            | _ -> false 
        | _ -> false

    let findDirection ((x, y) : coord) (placedTiles : Map<coord, (uint32 * (char * int))>) : coord =
        let xDirection = 
            match Map.tryFind (x+1, y) placedTiles, Map.tryFind (x-1, y) placedTiles with
            | None, None -> 
                match checkDirection (x+1, y) (1, 0) placedTiles with
                | true -> (1,0)
                | _ -> (0,0)
            | _ -> (0,0)
        let yDirection = 
            match Map.tryFind (x, y+1) placedTiles, Map.tryFind (x, y-1) placedTiles with
            | None, None -> 
                match checkDirection (x, y+1) (0, 1) placedTiles with
                | true -> (0,1)
                | _ -> (0,0)
            | _ -> (0,0)
        if xDirection <> (0,0) then xDirection else yDirection

    let uintListToMoveList (uintList : uint32 List) (coord : coord) (direction : coord) (pieces : Map<uint32, tile> ) : movesInWordList =
        let moveList = 
            List.fold (fun (acc, coord) id ->
                match Map.tryFind id pieces with
                | Some piece ->
                    let letter = piece |> Set.minElement |> fst
                    let pointValue = piece |> Set.minElement |> snd
                    let move = (coord, ((id), (letter, pointValue)))
                    let nextCoord = (((fst coord) + (fst direction)),((snd coord) + (snd direction)))
                    (acc @ [move], nextCoord)
                | None -> (acc, coord)
            ) ([], coord) uintList        
        fst moveList

    let rec buildWords (wordUntilNow : movesInWordList) (hand : MultiSet.MultiSet<uint32>) (dict : Dictionary.Dict) 
        ((x, y) : coord) ((dx, dy) : coord) (words : movesInWordList List) (pieces : Map<uint32, tile>) (st : State.state): movesInWordList List =
        // Check whether hand is empty or not
        if MultiSet.isEmpty hand then
            words
        else 
            // Fold through rest of hand
            MultiSet.fold (fun (acc, (x',y')) id _ ->
                // Remove letter from hand
                let newHand = MultiSet.removeSingle id hand
                match Map.tryFind id pieces with
                | Some piece ->
                    let idAsChar = 
                        match st.placedTiles |> Map.tryFind (x'+dx, y'+dy) with
                        | Some ((id, (char, _))) when id = 0u -> char // If the tile is a wildcard, use the character it is representing
                        | _ -> 
                            if id = 0u then 'A' 
                            else piece |> Set.minElement |> fst
                    let pointValue = piece |> Set.minElement |> snd
                    let move = ((x', y'), ((id), (idAsChar, pointValue)))
                    let nextCoord = (x'+dx, y'+dy) // Calculate next coord

                    // Check direction for nextCoord
                    if checkDirection nextCoord (dx, dy) st.placedTiles then
                        // Add move to movesList
                        let wordUntilNow' = List.append wordUntilNow [move]

                        // find child
                        let child = Dictionary.step idAsChar dict
                        match child with
                        | Some (isWord, newDict) -> 
                            let words' =
                                match isWord with
                                | true ->  
                                    wordUntilNow'::acc
                                | false ->
                                    acc
                            let newWords = buildWords wordUntilNow' newHand newDict nextCoord (dx, dy) words' pieces st
                            (newWords, (x',y'))
                        | None -> 
                            (acc, (x',y')) 
                    else (acc, (x',y')) 
                | None -> (acc, (x',y'))             
            ) (words, (x,y)) hand |> fst


    let makeFirstWordList (st : State.state) (pieces : Map<uint32, tile>) : movesInWordList =
        let words =
            // Fold through first letters in hand.
            MultiSet.fold (fun acc tileId _ -> 
                // Remove letter from hand
                let newHand = MultiSet.removeSingle tileId st.hand
                
                let letterAsChar = 
                    if tileId = 0u then 'A' 
                    else uintToChar tileId

                // Step first letter and get dictionary
                let firstLetterDict : Dictionary.Dict = ((false, st.dict), Dictionary.step letterAsChar st.dict) ||> Option.defaultValue |> snd

                // Save word so far
                let wordSoFar = [tileId]
                let movesSoFar = uintListToMoveList wordSoFar st.board.center (1,0) pieces

                // Build words.
                let wordsFromFirstLetter = buildWords movesSoFar newHand firstLetterDict (fst st.board.center+1, snd st.board.center) (1,0) [] pieces st
                wordsFromFirstLetter @ acc
            ) [] st.hand

        // Find longest word in words
        let longestWord = 
            if words.IsEmpty then [] 
            else words |> List.maxBy List.length
        longestWord
        
    let makeSubsequentWordList (st : State.state) (pieces : Map<uint32, tile>) : movesInWordList =
        let tilesToPlayFrom = 
            Map.fold (fun acc coord tile ->
                let tileId = fst tile 
                match findDirection coord st.placedTiles with 
                | (0,0) -> acc
                | dir -> (tileId, (coord, dir))::acc
            ) [] st.placedTiles

        let words = 
            List.fold (fun acc tile ->
                let tileId = fst tile
                let coord = snd tile |> fst
                let dir = snd tile |> snd

                let tileChar = 
                    match st.placedTiles |> Map.tryFind coord with
                        | Some ((id, (char, _))) when id = 0u -> char // If the tile is a wildcard, use the character it is representing
                        | _ -> 
                            if tileId = 0u then 'A' 
                            else uintToChar tileId

                // Step first character and get dictionary
                let firstCharDict: Dictionary.Dict = ((false, st.dict), Dictionary.step tileChar st.dict) ||> Option.defaultValue |> snd
                // Save word so far
                let wordSoFar = [tileId]
                let movesSoFar = uintListToMoveList wordSoFar coord dir pieces
                
                // Build words.
                let wordsFromFirstChar = buildWords movesSoFar st.hand firstCharDict (fst coord + fst dir, snd coord + snd dir) dir [] pieces st
                wordsFromFirstChar @ acc
            ) [] tilesToPlayFrom

        // Find longest word in words
        let longestWord = 
            if words.IsEmpty then [] 
            else words |> List.maxBy List.length
        // printfn "Longest word: %A\n" longestWord
        
        let longestWordMinusFirstLetter =
            match longestWord with
            | x::xs -> xs
            | _ -> []

        longestWordMinusFirstLetter

    let convertHandToList (hand : MultiSet.MultiSet<uint32>) : uint32 List =
        MultiSet.toList hand

    let playGame cstream pieces (st: State.state) =
        let rec aux (st: State.state) (isMyTurn : bool) =
            Print.printHand pieces (State.hand st)
            if  Map.isEmpty st.placedTiles && isMyTurn then
                debugPrint (sprintf "============ Trying to play first word of the game. ============\n")
                let move = makeFirstWordList st pieces
                if move.IsEmpty && (convertHandToList st.hand).Length = 7 then
                    send cstream (SMChange (convertHandToList st.hand))
                else if move.IsEmpty then
                    send cstream SMPass
                else
                    send cstream (SMPlay move)
            else if isMyTurn then
                debugPrint (sprintf "============ Trying to play any other word of the game. ============\n")
                // printfn "There is a word on the board, so we use second algo"
                let move = makeSubsequentWordList st pieces
                // printfn "Move.Length: %A\n" move.Length
                if move.IsEmpty && (convertHandToList st.hand).Length = 7 then 
                    send cstream (SMChange (convertHandToList st.hand))
                else if move.IsEmpty then
                    send cstream SMPass
                else
                    send cstream (SMPlay move)
            else 
                debugPrint("\n=======================\n**** OPPONENT TURN ****\n=======================\n")


            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM(CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                debugPrint (sprintf "============ Successful play by you. ============\n")
                let st' = st
                // Remove the tiles used from our hand
                let removePieces =
                    List.fold (fun acc (_, (id, _)) -> MultiSet.removeSingle id acc) st'.hand ms
                // Add new tiles to hand
                let addedPieces =
                    List.fold (fun acc (id, amount) -> MultiSet.add id amount acc) removePieces newPieces
                // Update placeTiles map
                let updatePlacedTiles =
                    List.fold (fun acc ((x, y), (id, (char, pv))) -> Map.add (x, y) (id, (char, pv)) acc) st'.placedTiles ms
                
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayers st.playerTurn addedPieces updatePlacedTiles
               
                aux st' (st.playerNumber % st.numberOfPlayers + 1u = st.playerNumber)
            | RCM(CMChangeSuccess(newTiles)) ->
                let newHand = 
                    List.fold (fun acc (id, amount) -> 
                        MultiSet.add id amount acc
                    ) MultiSet.empty newTiles
                
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayers st.playerTurn newHand st.placedTiles
                aux st' (st.playerNumber % st.numberOfPlayers + 1u = st.playerNumber)
            | RCM(CMPassed(pid)) ->
                debugPrint (sprintf "============ OTHER PLAYER PASSED ============\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
                
                // let st' = State.mkState st.board st.dict st.playerTurn st.numberOfPlayers st.playerTurn st.hand st.placedTiles
                aux st (pid % st.numberOfPlayers + 1u = st.playerNumber)
            | RCM(CMPlayed(pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                // printfn "Placed tiles before update %A \n" st.placedTiles.Count
                debugPrint (sprintf "============ CMPlayed ============\n")
                let updatePlacedTiles =
                    List.fold (fun acc ((x, y), (id, (char, pv))) -> Map.add (x, y) (id, (char, pv)) acc) st.placedTiles ms
                // printfn "Placed tiles after update %A \n" updatePlacedTiles.Count
                
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayers st.playerTurn st.hand updatePlacedTiles
                aux st' (pid % st.numberOfPlayers + 1u = st.playerNumber)
            | RCM(CMPlayFailed(pid, ms)) ->
                debugPrint (sprintf "============ CMPlayFailed ============\n")
                (* Failed play. Update your state *)
                aux st (pid % st.numberOfPlayers + 1u = st.playerNumber)
            | RCM(CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                if err.Head.Equals(GPENotEnoughPieces) then 
                    send cstream (SMPass)
                printfn "Gameplay Error:\n%A" err
                aux st (st.playerNumber % st.numberOfPlayers + 1u = st.playerNumber)
        aux st (st.playerTurn = st.playerNumber)

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerNumber: uint32)
        (playerTurn: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                playerTurn
                hand
                timeout
        )

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers playerTurn handSet Map.empty)
