// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.

    // 7.1
    let pIntToChar = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt = pstring "charToInt"
    let pToUpper = pstring "toUpper"
    let pToLower = pstring "toLower"
    let pCharValue = pstring "charValue"

    let pTrue = pstring "true"
    let pFalse = pstring "false"
    let pIsDigit = pstring "isDigit"
    let pIsLetter = pstring "isLetter"
    let pIsVowel = pstring "isVowel"

    let pif = pstring "if"
    let pthen = pstring "then"
    let pelse = pstring "else"
    let pwhile = pstring "while"
    let pdo = pstring "do"
    let pdeclare = pstring "declare"

    // 7.2
    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter = satisfy System.Char.IsLetter <?> "letter"

    let palphanumeric = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces = many whitespaceChar <?> "spaces"
    let spaces1 = many1 whitespaceChar <?> "space1"

    // 7.3
    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2 = p1 .>> spaces .>> p2
    let (>*>.) p1 p2 = p1 .>> spaces >>. p2

    // 7.4
    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

    // Bonus 7.4
    let tuborgparenthesise p = pchar '{' >*>. p .>*> pchar '}'

    // 7.5
    let charListToString (a: char list) = Array.ofList a |> System.String.Concat

    let pid =
        pletter <|> (pchar '_') .>>. many (palphanumeric <|> (pchar '_'))
        |>> (fun (x, xs) -> x :: xs |> charListToString)

    // 7.6
    let unop op a = op >*>. a

    // 7.7
    let binop op a b = a .>*> op .>*>. b

    // 7.8 & 7.9
    let TermParse, tref = createParserForwardedToRef<aExp> ()
    let ProdParse, pref = createParserForwardedToRef<aExp> ()
    let AtomParse, aref = createParserForwardedToRef<aExp> ()
    let CharParse, cref = createParserForwardedToRef<cExp> ()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [ AddParse; SubParse; ProdParse ]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [ MulParse; DivParse; ModParse; AtomParse ]

    let NParse = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let VParse = pid |>> V <?> "Var"
    let PVParse = unop pPointValue (parenthesise TermParse) |>> PV <?> "PointValue"
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> (N -1, x)) |>> Mul <?> "Neg"

    let CharToIntParse =
        unop pCharToInt (parenthesise CharParse) |>> CharToInt <?> "CharToInt"

    do aref := choice [ NegParse; CharToIntParse; PVParse; NParse; VParse; ParParse ]

    // 7.9
    let CParse = pchar '\'' >>. anyChar .>> pchar '\'' |>> C <?> "Char"
    let CVParse = unop pCharValue (parenthesise TermParse) |>> CV <?> "CharValue"
    let ToUpperParse = unop pToUpper (parenthesise CharParse) |>> ToUpper <?> "ToUpper"

    let ToLowerParse = unop pToLower (parenthesise CharParse) |>> ToLower <?> "ToLower"

    let IntToCharParse =
        unop pIntToChar (parenthesise TermParse) |>> IntToChar <?> "IntToChar"

    do cref := choice [ CParse; CVParse; ToUpperParse; ToLowerParse; IntToCharParse ]

    let AexpParse = TermParse
    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

     (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    let mkBoard (bp: boardProg) : board = failwith "not implemented"
