module Dictionary

    open ScrabbleUtil
    open ScrabbleUtil.Dictionary

    type Dict = 
        | Leaf of bool // In a dictionary built using the insert-function all branches end in a Leaf that is true.
        | Node of bool * System.Collections.Generic.Dictionary<char, Dict> 

    type csDict = System.Collections.Generic.Dictionary<char, Dict> // Basically just a shortcut so we don't have to write this every time.

    // Used to ensure we build the entire tree before terminating (terminate when word.Length = 0)
    let empty () = Leaf false 

    // Builds our dictionary by inserting words into it
    let rec insert (word : string) (d : Dict) = 
        match d with
        // When the last letter of word has been inserted, and the sub-dictionary is a Leaf. 
        | Leaf _ when word.Length = 0 -> Leaf true 
        // When the last letter of word has been inserted, and the sub-dictionary is a Node.
        | Node (_, dic) when word.Length = 0 -> Node (true, dic)
        | Leaf b -> 
            let newdic = csDict () 
            let c = word.[0] // Get first letter of word
            newdic.[c] <- insert word.[1..] (empty ()) // Insert rest of word under the first letter in the new dictionary.
            Node (b, newdic) // Create a node with the bool val from Leaf and the new dictionary. This Node replaces Leaf
        | Node (b, dic) -> 
            let c = word.[0] // Get first letter of word
            match dic.TryGetValue c with // Look for first letter in dictionary
            // If first letter is in dictionary - stay on path
            | (true, value) -> 
                dic.[c] <- insert word.[1..] value // Insert rest of word under the first letter in the sub-dictionary.
                Node (b, dic) // Update the node with the updated dictionary
            // If first letter is not in dictionary - branch out from path
            | (false, _) -> 
                dic.[c] <- insert word.[1..] (empty ()) // Insert rest of word under the first letter as a new sub-dictionary.
                Node (b, dic) // Update the node with the updated dictionary

    // Checks if word exists in dictionary
    let rec lookup (word : string) (d : Dict) = 
        match d with 
        | Leaf b when word.Length = 0 -> b // Return bool val. If True it was a word, if False not a word.
        | Node (b, _) when word.Length = 0 -> b  // Return bool val. If True it was a word, if False not a word.
        | Leaf _ -> false // Leaf hit before end of word, no words can be build from current point in word.
        // Node hit, check for sub-dictionary starting from first letter of word.
        | Node (b, dic) -> 
            match dic.TryGetValue word.[0] with 
            | (true, value) -> lookup word.[1..] value // Sub-dictionary exists from first letter of word, repeat lookup-function with remainder of word in sub-dictionary.
            | (false, _) -> false // No sub-dictionary from first letter of word, means no words exist starting with that letter.

    // Builds words character by character, using letters on board or in hand
    let step (c : char) (d : Dict) = 
        match d with 
        // If stepping onto Leaf, no words can be built from this letter.
        | Leaf _ -> None
        // If stepping onto Node, check if letter is in dictionary.
        | Node (_, dic) -> 
            match dic.TryGetValue c with 
            // If letter is in dictionary, check if it's a word, by looking at sub-dictionary. 
            // (No matter whether sub-dictionary is Leaf or Node)
            | (true, value) ->
                match value with 
                | Leaf b -> Some (b, value)
                | Node (b, _) -> Some (b, value)
            // If letter is not in dictionary, no words can be built from letter.
            | (false, _) -> None