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
    type moveList = move List

    let rec findFirstWord (hand: MultiSet.MultiSet<uint32>) (board : Parser.board) (accWord : uint32 List) (dict : Dictionary.Dict) (longestWord: uint32 List) (pieces : Map<uint32, tile>) (coord : coord) (direction : coord) : moveList =
        debugPrint "entering findFirstWord\n"
        // Convert hand to list
        let handList = MultiSet.toList hand
        printfn "Printing handList\n %A" handList
        let word =
            debugPrint "Finding longest word\n"
            List.fold (fun (longestWord : list<uint32>) letter -> 
                match handList with 
                | [] ->
                    printfn "handList is empty returning longestWord\n" 
                    longestWord
                | handList -> 
                    printfn "handList not empty checking for further words\n"
                    let child = Dictionary.step (uintToChar letter) dict
                    printfn "Child: %A\n" child
                    printfn "Char we are stepping with %A\n" letter

                    match child with
                    | Some (bool, newDict) -> 

                        let newCoord = ((fst coord) + (fst direction), (snd coord) + (snd direction))
                        printfn "New coord: %A\n" newCoord
                        
                        printfn "Adding %A to current word\n" letter 
                        // Add letter to accumulative word
                        let currentWord = letter::accWord
                        let currentWordInChars =
                            List.fold (fun acc letter -> (uintToChar letter)::acc) [] currentWord
                        printfn "Current word: %A\n" currentWordInChars

                        // remove letter we looked at from hand 
                        let newHand = MultiSet.removeSingle letter hand
                        printfn "New hand: %A\n" newHand

                        // Build possible words
                        let words = findFirstWord newHand board currentWord newDict (if currentWord.Length > longestWord.Length then currentWord else longestWord) pieces newCoord direction
                        printfn "words: %A\n" words

                        // Check if bool is true to see if currentWord is a playable word
                        match bool with
                        | true when currentWord.Length > longestWord.Length -> 
                            printfn "It is a word: %A\n" currentWordInChars
                            currentWord
                        | false ->
                            printfn "It is not a word\n"
                            longestWord
                        | _ -> 
                            printfn "It is a shorter word than current longest word\n"
                            longestWord
                    | None _ -> longestWord
            ) longestWord handList
        let wordInChars =
            List.fold (fun acc letter -> (uintToChar letter)::acc) [] word
        printfn "Found longest word: %A\n" wordInChars
        []


    // let placeFirstWord (st : State.state) (pieces : Map<uint32, tile>) : moveList =
    //     forcePrint "Entering placeFirstWord\n"
    //     let findFirstWord (st : State.state) (pieces : Map<uint32, tile>) : moveList List =
    //         forcePrint "Entering findFirstWord\n"
    //         let rec aux ((x,y):coord) (hand:MultiSet.MultiSet<uint32>) (dict:Dictionary.Dict) (playableWords:moveList List) (movesUntilNow:moveList)  =
    //             printfn "Entering aux with coord: %A, hand: %A\n" (x, y) hand
    //             match MultiSet.toList hand with
    //             | [] ->
    //                 printfn "Hand is empty, returning playableWords: %A\n" playableWords
    //                 playableWords
    //             | handList ->
    //                 printfn "Hand is not empty, processing handList: %A\n" handList
    //                 let chars = List.map (fun id -> uintToChar id) handList
    //                 List.fold (fun acc c ->
    //                     printfn "Processing character: %A\n" c
    //                     match Dictionary.step c dict with
    //                     | None ->
    //                         printfn "Dictionary step returned None, returning acc: %A\n" acc
    //                         acc
    //                     | Some (b, cDict) ->
    //                         printfn "Dictionary step returned Some: %A\n" (b, cDict)
    //                         if (b) then
    //                             let nextCoord = (x+1, y)
    //                             let id = charToUint c
    //                             let wordSoFar : moveList = ((x,y), (id, Set.minElement (Map.find id pieces)))::movesUntilNow
    //                             let playableWords = wordSoFar :: playableWords
    //                             printfn "Found a word, adding to playableWords calling aux with nextCoord: %A, wordSoFar: %A\n" nextCoord wordSoFar
    //                             aux nextCoord (MultiSet.removeSingle (charToUint c) hand) cDict playableWords wordSoFar
    //                         else
    //                             let nextCoord = (x+1, y)
    //                             printfn "Did not find a word, calling aux with nextCoord: %A\n" nextCoord
    //                             aux nextCoord (MultiSet.removeSingle (charToUint c) hand) cDict playableWords movesUntilNow
    //                 ) playableWords chars
    //         // Start aux'en med tomme lister
    //         printfn "Starting aux with initial coord: (0,0), hand: %A\n" st.hand
    //         aux (0,0) st.hand st.dict List.empty List.empty
    //     // Find længste ord fundet med findFirstWord
    //     let result = List.maxBy List.length (findFirstWord st pieces)
    //     forcePrint (sprintf "placeFirstWord result: %A" result)
    //     result

    // Code to figure out if we are playing the first word or just a any other word.
    // let playWord (st : State.state) pieces =
    //     match st.placedTiles.Count with
    //     | 0 -> findFirstWord st pieces |> List.maxBy List.length
    //     | _ -> findContinuationWord st pieces |> List.maxBy List.length

    // This was code made with Fink
    // let placeFirstWord (st : State.state) pieces :  moveList List  =
    //     // start i 0,0 kig mod højre
    //     let rec aux ((x,y):coord) (possibleMoves:moveList List) (movesUntilNow:moveList) (dict:Dictionary.Dict) (hand:MultiSet.MultiSet<uint32*tile>) =
    //         match MultiSet.toList hand with
    //             | [] -> possibleMoves
    //             | handList -> List.fold (fun acc ((id,(tile:tile))) ->
    //                             Set.fold (fun acc (c,pv) ->
    //                                 match Dictionary.step c dict with
    //                                 | None -> possibleMoves
    //                                 | Some(bool,newDict) ->
    //                                     if (bool) then
    //                                         let nextCoord = (x+1, y)
    //                                         aux nextCoord acc (((nextCoord, id))::movesUntilNow) newDict (MultiSet.removeSingle id hand)
    //                                     else acc
    //                                 ) acc tile
    //                             ) possibleMoves handList
    //     aux (0,0) List.empty List.empty st.dict st.hand


    let playGame cstream pieces (st: State.state) =

        let rec aux (st: State.state) =
            Print.printHand pieces (State.hand st)

            if st.playerNumber = 1u && Map.isEmpty st.placedTiles then
                let test = findFirstWord st.hand st.board [] st.dict [] pieces (0,0) (1,0)
                let input = System.Console.ReadLine()
                let move = RegEx.parseMove input
                send cstream (SMPlay test)
            else
                forcePrint
                    "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

                let input = System.Console.ReadLine()
                let move = RegEx.parseMove input
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
