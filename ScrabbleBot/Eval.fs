﻿// File has been updated with our own

module internal Eval

    open StateMonad
    
    (* Code for testing *)

    let hello = [('H', 4) ; ('E', 1) ; ('L', 1) ; ('L', 1) ; ('O',1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []


    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)
    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let add (a: SM<int>) (b: SM<int>) = a >>= (fun x -> b >>= (fun y -> ret (x+y)))
    let sub (a: SM<int>) (b: SM<int>) = a >>= (fun x -> b >>= (fun y -> ret (x-y)))
    let mul (a: SM<int>) (b: SM<int>) = a >>= (fun x -> b >>= (fun y -> ret (x*y)))
    let modu a b = a >>= (fun x -> b >>= (fun y -> if (y = 0) then fail (DivisionByZero) else ret (x%y)))
    let aeq a b = a >>= (fun x -> b >>= (fun y -> ret (x=y)))
    let alt a b = a >>= (fun x -> b >>= (fun y -> ret (x<y)))
    let conj a b = a >>= (fun x -> b >>= (fun y -> ret (x&&y)))
    let div a b = a >>= (fun x -> b >>= (fun y -> if (y = 0) then fail (DivisionByZero) else ret (x/y))) 
    let isVowel c =
        ['A'; 'E'; 'I'; 'O'; 'U'] 
        |> List.exists (fun d -> c = d || c = System.Char.ToLower d)

    let rec arithEval (a: aExp) : SM<int> =
        match a with
        | WL -> wordLength
        | PV x -> (arithEval x) >>= pointValue
        | V x -> lookup x
        | Add (x, y) -> add (arithEval x) (arithEval y)
        | Sub (x, y) -> sub (arithEval x) (arithEval y)
        | Mul (x, y) -> mul (arithEval x) (arithEval y)
        | Mod (x, y) -> modu (arithEval x) (arithEval y)
        | Div (x, y) -> div (arithEval x) (arithEval y)
        | N x -> ret x
        | CharToInt x -> charEval x >>= (fun a -> ret(int a))
    and charEval (c: cExp) : SM<char> = 
        match c with
        | C x -> ret x  
        | CV x -> (arithEval x) >>= characterValue 
        | ToUpper x -> charEval x >>= (fun a -> ret (System.Char.ToUpper a))
        | ToLower x -> charEval x >>= (fun a -> ret (System.Char.ToLower a))
        | IntToChar x -> arithEval x >>= (fun a -> ret (char a))
    and boolEval (b: bExp) : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (x, y) -> aeq (arithEval x) (arithEval y)
        | ALt (x, y) -> alt (arithEval x) (arithEval y)
        | Not x -> boolEval x >>= (fun a -> ret (not a))
        | Conj (x, y) -> conj (boolEval x) (boolEval y)
        | IsDigit x -> charEval x >>= (fun a -> ret (System.Char.IsDigit a))
        | IsLetter x -> charEval x >>= (fun a -> ret (System.Char.IsLetter a))
        | IsVowel x -> charEval x >>= (fun a -> ret (isVowel a))


    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare x -> declare x
        | Ass (x, y) -> ((arithEval y) >>= (fun a -> update x a))
        | Skip -> ret ()
        | Seq (x, y) -> stmntEval x >>>= stmntEval y
        | ITE (x, y, z) -> push >>>= (boolEval x >>= (fun a -> if a then stmntEval y else stmntEval z))
        | While (x, y) -> push >>>= (boolEval x >>= (fun a -> if a then stmntEval y >>>= stmntEval (While (x, y)) else ret ()))

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    let mkBoard c x boardStmnt ids = failwith "Not implemented"
    