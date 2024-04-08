module Dictionary

    open ScrabbleUtil
    open ScrabbleUtil.Dictionary

    type Dict = 
        | Leaf of bool 
        | Node of bool * System.Collections.Generic.Dictionary<char, Dict>

    type csDict = System.Collections.Generic.Dictionary<char, Dict>

    let empty () = Leaf false

    let rec insert (word : string) (d : Dict) = 
        match d with
        | Leaf _ when word.Length = 0 -> Leaf true
        | Node (_, dic) when word.Length = 0 -> Node (true, dic)
        | Leaf b -> 
            let newdic = csDict ()  
            let c = word.[0] 
            newdic.[c] <- insert word.[1..] (empty ())
            Node (b, newdic)
        | Node (b, dic) -> 
            let c = word.[0]
            match dic.TryGetValue c with
            | (true, value) -> 
                dic.[c] <- insert word.[1..] value 
                Node (b, dic)
            | (false, _) -> 
                dic.[c] <- insert word.[1..] (empty ())
                Node (b, dic)

    let rec lookup (word : string) (d : Dict) = 
        match d with 
        | Leaf b when word.Length = 0 -> b
        | Node (b, _) when word.Length = 0 -> b
        | Leaf _ -> false
        | Node (b, dic) -> 
            match dic.TryGetValue word.[0] with 
            | (true, value) -> lookup word.[1..] value
            | (false, _) -> false

    let step (c : char) (d : Dict) = 
        match d with 
        | Leaf _ -> None
        | Node (_, dic) -> 
            match dic.TryGetValue c with 
            | (true, value) ->
                match value with 
                | Leaf b -> Some (b, value)
                | Node (b, _) -> Some (b, value)
            | (false, _) -> None