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

    let mutable userInput = System.Console.ReadLine ()
    while not (validateUserCode userInput) do
        printfn "Invalid Input"
        printfn "%s" invalidInputString
        userInput <- System.Console.ReadLine ()
    let mutable (code: code) = []
    for i = 0 to 3 do
        code <-(charToCol userInput.[i]) :: code
    code




let getComputerCode =
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
    for i = 0 to 3 do
        code <- (intToCol (random.Next(1,6))) :: code
    code



let makeCode (player1: player) =
    match player1 with
    | Computer -> getComputerCode
    | Human ->
        printfn "Please input the secret code"
        getUserCode "Please input the secret code"

let guess (player2: player) (gameBoard: board) =
    printfn "Gameboard:"
    for i = 0 to gameBoard.Length - 1 do
        printfn "%A" gameBoard.[i]
    match player2 with
    | Computer -> getComputerCode
    | Human ->
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
    let mutable playerInput  = System.Console.ReadLine ()
    while not (validatePlayerInput playerInput) do
        printfn "Invalid Input"
        printfn "Please input player %d (C/H): " i
        playerInput <- System.Console.ReadLine ()
    inputToPlayer playerInput


let validate (secretCode: code) (playerGuess: code) =
    ()


let playGame =
    let player1 = getPlayer 1
    let player2 = getPlayer 2
    let secretCode = makeCode player1
    let mutable (gameBoard: board) = []
    let mutable (playerGuess: code) = []
    let mutable (answer: answer) = (0, 0)
    let mutable counter = 0
    while playerGuess <> secretCode do
        playerGuess <- guess player2 gameBoard
        answer <- validate secretCode playerGuess
        printfn "%A" answer
        gameBoard <- (playerGuess, answer) :: gameBoard
        counter <- counter + 1
    printfn "Congratulations you've won!"
    printfn "you made %d guesses" counter
    printfn "This is your gameboard:"
    for i = 0 to gameBoard.Length - 1 do
        printfn "%A" gameBoard.[i]
