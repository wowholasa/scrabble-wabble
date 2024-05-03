// Has been replaced with our implementation
module internal MultiSet

type MultiSet<'a when 'a: comparison> = R of Map<'a, uint32> // replace with your type

// Green exercises
let empty = R(Map.empty)

let isEmpty (R s: MultiSet<'a>) = s.IsEmpty

let size (R a) = Seq.sum a.Values

let contains (a: 'a) (R s: MultiSet<'a>) = Seq.contains a s.Keys

let numItems (a: 'a) (R s: MultiSet<'a>) =
    if contains a (R s) then s.Item a else 0u

let add (a: 'a) (n: uint32) (R s: MultiSet<'a>) : MultiSet<'a> =
    match Map.tryFind a s with
    | None -> R(Map.add a n s)
    | _ -> R(Map.add a ((s.Item a) + n) s)

let addSingle (a: 'a) (s: MultiSet<'a>) : MultiSet<'a> = add a 1u s

let remove (a: 'a) (n: uint32) (R s: MultiSet<'a>) : MultiSet<'a> =
    match Map.tryFind a s with
    | None -> R s
    | Some x when (numItems a (R s)) <= n -> R(Map.remove a s)
    | _ -> R(Map.add a ((s.Item a) - n) s)

let removeSingle (a: 'a) (s: MultiSet<'a>) : MultiSet<'a> = remove a 1u s 


let fold (f: 'a -> 'b -> uint32 -> 'a) (x: 'a) (R s: MultiSet<'b>) = Map.fold f x s

let foldBack (f: 'a -> uint32 -> 'b -> 'b) (R s: MultiSet<'a>) (x: 'b) = Map.foldBack f s x

// Yellow exercises
let ofList (lst: 'a list) : MultiSet<'a> = failwith ""
let toList (R(s) : MultiSet<'a>) : 'a list =
        (s,[]) ||> Map.foldBack (fun a b lst -> [for _ in 1u .. b -> a] @ lst)


let map (_: 'a -> 'b) (_: MultiSet<'a>) : MultiSet<'b> = failwith "" // R ()

let union (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = failwith "" // R ()
let sum (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = failwith "" // R ()

let subtract (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = failwith "" // R ()

let intersection (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = failwith "" // R ()
