﻿namespace ScrabbleWabble

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
          hand: MultiSet.MultiSet<uint32>
          placedTiles: Map<coord, uint32> }

    let mkState b d pn h pt =
        { board = b
          dict = d
          playerNumber = pn
          hand = h
          placedTiles = pt }

    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand
    let placedTiles st = st.placedTiles

module Scrabble =
    open System.Threading

    type move = (coord * (uint32 * (char * int)))
    type movesInWordList = move List

    let checkDirection ((x, y) : coord) (direction : coord) (placedTiles : Map<coord, uint32>) : bool =
        // printfn "Entering checkDirection\n"
        match direction with
        | (1,0) -> // Moving right
            match Map.tryFind (x+1, y) placedTiles with 
            | None when Map.tryFind (x, y+1) placedTiles = None && Map.tryFind (x, y-1) placedTiles = None -> true
            | _ -> 
                // printfn "Hit false in x direction \n"
                false
        | (0,1) -> // Moving down
            match Map.tryFind (x, y+1) placedTiles with
            | None when Map.tryFind (x+1, y) placedTiles = None && Map.tryFind (x-1, y) placedTiles = None -> true
            | _ -> 
                // printfn "Hit false in y direction \n"
                false 
        | _ -> false

    let findDirection ((x, y) : coord) (placedTiles : Map<coord, uint32>) : coord =
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
        // printfn "Entering buildWords \n"
        // printfn "Printing hand in buildWords %A\n" hand

        // let wordCount = words.Length
        // printfn "Found %A words\n" wordCount
        // printfn "All found words: %A\n" words

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
                    let idAsChar = piece |> Set.minElement |> fst
                    // printfn "Stepping with letter %A\n" idAsChar
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
                                    // printfn "This is a word, WordUntilNow' is currently: %A\n" currentWordInChars
                                    wordUntilNow'::acc
                                | false ->
                                    // printfn "This is not a word \n" 
                                    acc
                            let newWords = buildWords wordUntilNow' newHand newDict nextCoord (dx, dy) words' pieces st
                            (newWords, (x',y'))
                        | None -> 
                            // printfn "Entering none in match child\n"
                            (acc, (x',y')) 
                    else (acc, (x',y')) 
                | None -> (acc, (x',y'))             
            ) (words, (x,y)) hand |> fst


    let makeFirstWordList (st : State.state) (pieces : Map<uint32, tile>) : movesInWordList =
        // printfn "Entering makeFirstWordList\n"
        // Initialising the list we want to return (for clarity)
        let words =
            // Fold through first letters in hand.
            MultiSet.fold (fun acc letter _ -> 
                // Remove letter from hand
                let newHand = MultiSet.removeSingle letter st.hand

                let letterAsChar = uintToChar letter
                // printfn "Building words with first letter: %A\n" letterAsChar

                // Step first letter and get dictionary
                let firstLetterDict : Dictionary.Dict = ((false, st.dict), Dictionary.step letterAsChar st.dict) ||> Option.defaultValue |> snd

                // Save word so far

                let wordSoFar = [letter]
                let movesSoFar = uintListToMoveList wordSoFar st.board.center (1,0) pieces

                // let currentWordInChars = List.fold (fun acc letter -> acc @ [(uintToChar letter)]) [] wordSoFar
                // printfn "The word so far in chars %A\n" currentWordInChars

                // Build words.
                let wordsFromFirstLetter = buildWords movesSoFar newHand firstLetterDict (fst st.board.center+1, snd st.board.center) (1,0) [] pieces st
                wordsFromFirstLetter @ acc
            ) [] st.hand
        let wordCount = words.Length
        printfn "Found %A words\n" wordCount
        // printfn "All found words: %A\n" words

        // Find longest word in words
        let longestWord = words |> List.maxBy List.length
        printfn "Longest word: %A\n" longestWord
        
        longestWord
        
    let makeSubsequentWordList (st : State.state) (pieces : Map<uint32, tile>) : movesInWordList =
        // printfn "Entering makeSubSequentWord \n"
        let tilesToPlayFrom = 
            // printfn "Entering tilesToPlayFrom \n"
            Map.fold (fun acc coord tileId -> 
                match findDirection coord st.placedTiles with 
                | (0,0) -> acc
                | dir -> (tileId, (coord, dir))::acc
            ) [] st.placedTiles
        // printfn "tilesToPlayFrom length: %A\n" tilesToPlayFrom.Length

        let words = 
            // printfn "Entering words in makeSubsequentWord \n"
            List.fold (fun acc tile ->
                // printfn "Entering fold statement in words in makeSub \n"
                let tileId = fst tile
                // printfn "tileId is %A \n" tileId
                let coord = snd tile |> fst
                let dir = snd tile |> snd

                let tileChar = uintToChar tileId

                // Step first character and get dictionary
                let firstCharDict: Dictionary.Dict = ((false, st.dict), Dictionary.step tileChar st.dict) ||> Option.defaultValue |> snd
                // Save word so far
                let wordSoFar = [tileId]
                let movesSoFar = uintListToMoveList wordSoFar coord dir pieces
                
                // Build words.
                let wordsFromFirstChar = buildWords movesSoFar st.hand firstCharDict (fst coord + fst dir, snd coord + snd dir) dir [] pieces st
                wordsFromFirstChar @ acc
            ) [] tilesToPlayFrom
        let wordCount = words.Length
        printfn "Found %A words\n" wordCount
        //printfn "All found words: %A\n" words

        // Find longest word in words
        let longestWord = words |> List.maxBy List.length
        printfn "Longest word: %A\n" longestWord
        
        let longestWordMinusFirstLetter =
            match longestWord with
            | x::xs -> xs
            | _ -> failwith "this is so wrong, but it feels so right"

        longestWordMinusFirstLetter


    // Code to figure out if we are playing the first word or just a any other word.
    // let playWord (st : State.state) pieces =
    //     match st.placedTiles.Count with
    //     | 0 -> findFirstWord st pieces |> List.maxBy List.length
    //     | _ -> findContinuationWord st pieces |> List.maxBy List.length

    let playGame cstream pieces (st: State.state) =
        printfn "Entering playGame \n"
        let rec aux (st: State.state) =
            Print.printHand pieces (State.hand st)

            if st.playerNumber = 1u && Map.isEmpty st.placedTiles then
                let move = makeFirstWordList st pieces
                send cstream (SMPlay move)
            else
                // forcePrint
                //     "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                // let input = System.Console.ReadLine()
                // let move = RegEx.parseMove input
                let move = makeSubsequentWordList st pieces
                send cstream (SMPlay move)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // let input = System.Console.ReadLine()
            // let move = RegEx.parseMove input

            // debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            // send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM(CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = st
                // Remove the tiles used from our hand
                let removePieces =
                    List.fold (fun acc (_, (id, _)) -> MultiSet.removeSingle id acc) st'.hand ms
                // debugPrint "\n hand print after remove pieces is called\n"
                // Print.printHand pieces (State.hand st')

                // Add new tiles to hand
                let addedPieces =
                    List.fold (fun acc (id, amount) -> MultiSet.add id amount acc) removePieces newPieces
                // debugPrint "\n Hand print after pieces is added\n"
                // Print.printHand pieces (State.hand st')

                // Update placeTiles map
                let updatePlacedTiles =
                    List.fold (fun acc ((x, y), (id, _)) -> Map.add (x, y) id acc) st'.placedTiles ms

                let st' =
                    { st with
                        hand = addedPieces
                        placedTiles = updatePlacedTiles }
                // debugPrint "\n Printing map of placedTiles\n"
                // Map.iter (fun (x, y) id -> printfn "(%d, %d): %d" x y id) st'.placedTiles
                aux st'
            | RCM(CMPlayed(pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM(CMPlayFailed(pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM(CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st


        aux st

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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty)
