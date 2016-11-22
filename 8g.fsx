type codeColour =
    Red | Green | Yellow | Purple | White | Black
type code = codeColour list
type answer = int * int
type board = ( code * answer ) list
type player = Human | Computer


let validateUserCode (userInput: string) =
    let validateColour (userColour: char) =
        match userColour with
        | 'R' -> true
        | 'G' -> true
        | 'Y' -> true
        | 'P' -> true
        | 'W' -> true
        | 'B' -> true
        |  _  -> false
    userInput.Length = 4 && (validateColour userInput.[0]) && (validateColour userInput.[1]) && (validateColour userInput.[2]) && (validateColour userInput.[3])




let getUserCode (invalidInputString: string)=
    let charToCol (inputColour: char) =
        match inputColour with
        | 'R' -> Red
        | 'G' -> Green
        | 'Y' -> Yellow
        | 'P' -> Purple
        | 'W' -> White
        | 'B' -> Black
        |  _  -> Black

    let mutable userInput = System.Console.ReadLine().ToUpper()
    while not (validateUserCode userInput) do
        printfn "Invalid Input"
        printfn "%s" invalidInputString
        userInput <- System.Console.ReadLine().ToUpper()
    let mutable (code: code) = []
    for i = userInput.Length - 1  downto 0 do
        code <-(charToCol userInput.[i]) :: code
    code

let getComputerCode () =
    let intToCol (i: int) =
        match i with
        | 0 -> Red
        | 1 -> Green
        | 2 -> Yellow
        | 3 -> Purple
        | 4-> White
        | 5 -> Black
        | _ -> Black
    let random = new System.Random()
    let mutable (code: code) = []
    let mutable rndNum = 0
    for i = 0 to 3 do
        rndNum <- random.Next(6)
        code <- (intToCol rndNum) :: code
    code

let computerAI (gameBoard: board) =
    let intToCol (i: int) =
        match i with
        | 1 -> Red
        | 2 -> Green
        | 3 -> Yellow
        | 4 -> Purple
        | 5 -> White
        | 6 -> Black
        | _ -> Black
    let random = new System.Random()
    let mutable (code: code) = []
    let mutable rndNum = 0
    for i = 0 to 3 do
        rndNum <- random.Next(6)
        code <- (intToCol rndNum) :: code
    while (List.filter (fun (x, y) -> x = code) gameBoard).Length > 0 do
        if (List.filter (fun (x, y) -> x = code) gameBoard).Length > 0 then
            code <- []
        for i = 0 to 3 do
            rndNum <- random.Next(6)
            code <- (intToCol rndNum) :: code
    code

let makeCode (player1: player) =
    match player1 with
    | Computer -> getComputerCode ()
    | Human ->
        printfn "Please input the secret code"
        getUserCode "Please input the secret code"

let guess (player2: player) (gameBoard: board) =
    match player2 with
    | Computer -> computerAI gameBoard
    | Human ->
        printfn "Gameboard: "
        for i = gameBoard.Length - 1 downto 0 do
            printfn "%A" gameBoard.[i]
        printfn ""
        printfn "please input your guess"
        getUserCode "please input your guess"


let validatePlayerInput (playerType: string) =
    match playerType with
    | "H" -> true
    | "C" -> true
    |  _  -> false


let getPlayer (i: int) =
    let inputToPlayer (input: string) =
        match input with
        | "C" -> Computer
        | "H" -> Human
        |  _  -> Computer

    printfn "please input player %d (C/H): " i
    let mutable playerInput  = System.Console.ReadLine().ToUpper()
    while not (validatePlayerInput playerInput) do
        printfn "Invalid Input"
        printfn "Please input player %d (C/H): " i
        playerInput <- System.Console.ReadLine().ToUpper()
    inputToPlayer playerInput


let colourCount (code: code) =
    let array = [|0; 0; 0; 0; 0; 0|]
    for i = 0 to code.Length - 1 do
        match code.[i] with
        | Red -> array.[0] <- (array.[0] + 1)
        | Green -> array.[1] <- (array.[1] + 1)
        | Yellow -> array.[2] <- (array.[2] + 1)
        | Purple -> array.[3] <- (array.[3] + 1)
        | White -> array.[4] <- (array.[4] + 1)
        | Black -> array.[5] <- (array.[5] + 1)
    array


let validate (secretCode: code) (playerGuess: code) =
    let mutable whites = 0
    let mutable blacks = 0
    let secretColours = colourCount secretCode
    let guessColours = colourCount playerGuess

    for i = 0 to 5 do
        if guessColours.[i] <= secretColours.[i] then
            whites <- whites + guessColours.[i]
        else
            whites <- whites + secretColours.[i]
    for j = 0 to secretCode.Length - 1 do
        if secretCode.[j] = playerGuess.[j] then
            blacks <- blacks + 1
            whites <- whites - 1
    (blacks, whites)






let playGame () =
    let player1 = getPlayer 1
    let player2 = getPlayer 2
    printfn ""
    let secretCode = makeCode player1
    let mutable (gameBoard: board) = []
    let mutable (playerGuess: code) = []
    let mutable (answer: answer) = (0, 0)
    let mutable counter = 0
    while playerGuess <> secretCode do
        printfn ""
        playerGuess <- guess player2 gameBoard
        answer <- validate secretCode playerGuess
        if player2 = Human then
            printfn "Answer: %A (B/W)" answer
        gameBoard <- (playerGuess, answer) :: gameBoard
        counter <- counter + 1
    printfn ""
    printfn "Congratulations you've won!"
    printfn "you made %d guesses" counter
    printfn "This is your gameboard:"
    for i = gameBoard.Length - 1 downto 0 do
        printfn "%A" gameBoard.[i]


playGame ()
