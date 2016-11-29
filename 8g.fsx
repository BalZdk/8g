type codeColour =
    Red | Green | Yellow | Purple | White | Black
type code = codeColour list
type answer = int * int
type board = ( code * answer ) list
type player = Human | Computer


let random = new System.Random()
///<summary>
/// This function is given a string and returns true if the string is a "valid" input
/// according to the rules of the game.
///</summary>
///<param name = "userInput">
/// A string that is checked according to game rules.
///</param>
///<remarks>
/// valid inputs are: strings of length 4 where each character is either R,G,Y,P,W or B
///</remarks>
///<returns>
/// This function returns true if the given string is valid according to the game rules
/// and returns false otherwise.
///</returns>
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




///<summary>
/// This function asks the user for input, then chekecs if the input is valid.
/// if it isn't valid it'll ask for a new input, otherwise it'll turn the input
/// into a variable of type code.
///</summary>
///<param name = "invalidInputString">
/// A string which is printed if the input string is invalid.
///</param>
///<remarks>
/// This function turns all inputs in to uppercase letters,
/// thus allowing the user to write lowercase without the program calling errors.
///<returns>
/// This function returns a code created based on user input.
///</returns>
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



///<summary>
/// This function generates a random code.
///</summary>
///<returns>
/// This function returns a randomly generated code.
///</returns>
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
    let mutable (code: code) = []
    let mutable rndNum = 0
    for i = 0 to 3 do
        rndNum <- random.Next(6)
        code <- (intToCol rndNum) :: code
    code




///<summary>
/// This function calls getComputerCode to get a random code.
/// if the generated code is in the given gameBoard then it calles
/// getComputerCode to get a new code, then repeats untill it has a
/// code that isn't in the gameboard.
///</summary>
///<param name = "gameBoard">
/// a variable of type board that is used to check if the generated code
/// has been used before.
///</param>
///<returns>
/// This function returns a code that isn't in the gameBoard argument.
///</returns>
let computerAI (gameBoard: board) =
    let mutable code = getComputerCode ()
    while (List.filter (fun (x, y) -> x = code) gameBoard).Length > 0 do
        code <- getComputerCode ()
    code




///<summary>
/// This function uses get ComputerCode to generate a random code
/// if the given player is a Computer, if the given player is Human.
/// then it'll ask for an input and then calls getUserCode
///</summary>
///<param name = "player1">
/// This variable of type player decides how the returned code is generated.
///</param>
///<returns>
/// This function returns a variable of type code.
///</returns>
let makeCode (player1: player) =
    match player1 with
    | Computer -> getComputerCode ()
    | Human ->
        printfn "Please input the secret code"
        getUserCode "Please input the secret code"




///<summary>
///This function is given a player type and a gameboard,
/// and returns a code.
///</summary>
///<param name = "player2">
/// This player type decides how the returned code is generated.
///</param>
///<param name = "gameBoard">
/// This board is printed if the player type is Human,
/// ro is given to computerAI if the player type is Computer.
///</param>
///<returns>
/// This function returns a variable of type code.
///</returns>
let guess (player2: player) (gameBoard: board) =
    match player2 with
    | Computer -> computerAI gameBoard
    | Human ->
        printfn "Gameboard: "
        for i = gameBoard.Length - 1 downto 0 do
            printfn "%A" gameBoard.[i]
        printfn ""
        printfn "Please input your guess"
        getUserCode "Please input your guess"





///<summary>
/// This function returns true if the given string is "valid".
///</summary>
///<param name = "playerType">
/// This variable is tested for it's "validity"
///</param>
///<remarks>
/// The given string is valid if the given string is either "C" or "H"
///</remarks>
///<returns>
/// This function returns a type Boolean.
///</returns>
let validatePlayerInput (playerType: string) =
    match playerType with
    | "H" -> true
    | "C" -> true
    |  _  -> false





///<summary>
/// This function asks for user input, and returns the given input
/// as type player.
///</summary>
///<param name = "i">
/// This variable is used to ask for appropriate player number.
///</param>
///<remarks>
/// This function turns all inputs into uppercase, such that the user can write in lowercase without errors.
///</remarks>
///<returns>
/// This function returns type player, according to the input.
///</returns>
let getPlayer (i: int) =
    let inputToPlayer (input: string) =
        match input with
        | "C" -> Computer
        | "H" -> Human
        |  _  -> Computer

    printfn "Please input player %d (C/H): " i
    let mutable playerInput  = System.Console.ReadLine().ToUpper()
    while not (validatePlayerInput playerInput) do
        printfn "Invalid Input"
        printfn "Please input player %d (C/H): " i
        playerInput <- System.Console.ReadLine().ToUpper()
    inputToPlayer playerInput





///<summary>
/// This function takes a code and counts how many there are of each colour
/// and saves that number in an array,
/// such that for each red colour the first number of the array goes up by 1.
///</summary>
///<param name = "code">
/// This is the code where the function counts how many there are of each colour.
///</param>
///<returns>
/// This function returns an array that has information which
/// tells how many there are of each colour.
///</returns>
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





///<summary>
/// This function is given two codes, it then compares them and returns
/// a tuple describing how many colours are the same across the codes
/// and how many are in the exact same spot.
/// (The answer is of the same design as the mastermind boardgame.)
///</summary>
///<param name = "secretCode">
/// This is the "hidden" code in mastermind and is one of the two codes that shall be compared.
///</param>
///<param name = "playerGuess">
/// This is the guess done by player2 in mastermind, and is compared to secretCode.
///</param>
///<returns>
/// This function returns a tuple of two ints, which give information
/// as to how accurate the guess was.
///</returns>
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



///<summary>
/// Purty title card
///</summary>
///<returns>
/// You get NOTHING! You LOSE! Good day, sir!
///</returns>
let splash =
    "
        /|    /| ----------
       / |   / | a s t e r
      /  |  /  |
     /   | /   | i n d
    /    |/    | ----------

    "

///<summary>
/// This function runs the primary game loop and
/// and calls the above functions in the right order and right context.
///</summary>
///<returns>
/// This function returns nothing but just starts a loop that goes untill player 2 has won.
///</returns>
let playGame () =
    printfn "%s" splash
    System.Console.Beep(666,1000)
    let player1 = getPlayer 1
    let player2 = getPlayer 2
    printfn ""
    let secretCode = makeCode player1
    let mutable (gameBoard: board) = []
    let mutable (playerGuess: code) = []
    let mutable (answer: answer) = (0, 0)
    let mutable counter = 0
    while playerGuess <> secretCode do
        if player2 = Human then
            System.Console.Clear()
        playerGuess <- guess player2 gameBoard
        answer <- validate secretCode playerGuess
        if player2 = Human then
            printfn "Answer: %A (B/W)" answer
        gameBoard <- (playerGuess, answer) :: gameBoard
        counter <- counter + 1
    printfn ""
    printfn "Congratulations! A winner is you!"
    printfn "You made %d guesses" counter
    printfn "This is your gameboard:"
    for i = gameBoard.Length - 1 downto 0 do
        printfn "%A" gameBoard.[i]



let blackBoxTesting () =
    printfn "          ValidateUserCode:"
    printfn "test1: %b" (validateUserCode "XRGB" = false)
    printfn "test2: %b" (validateUserCode "RGYP" = true)
    printfn "test3: %b" (validateUserCode "RRRRR" = false)
    printfn "test4: %b" (validateUserCode "RG1R" = false)

    printfn "          validatePlayerInput:"
    printfn "test1: %b" (validatePlayerInput "cat" = false)
    printfn "test2: %b" (validatePlayerInput "C" = true)
    printfn "test3: %b" (validatePlayerInput "H" = true)
    printfn "test4: %b" (validatePlayerInput "c" = false)

    printfn "          colourCount:"
    printfn "test1: %b" (colourCount [Red; Green; Yellow; Green] = [|1; 2; 1; 0; 0; 0|])
    printfn "test2: %b" (colourCount [Red; Red; Red; Red; Red; Red] = [|6; 0; 0; 0; 0; 0|])
    printfn "test3: %b" (colourCount [White; White; Black; Black] = [|0; 0; 0; 0; 2; 2|])
    printfn "test4: %b" (colourCount [Red; Green; Yellow; Purple] = [|1; 1; 1; 1; 0; 0|])
    printfn "          validate:"
    printfn "test1: %b" (validate [Red; Red; Green; Black] [Red; Green; Yellow; Black] = (2, 1))
    printfn "test2: %b" (validate [Red; Red; Red; Red] [Green; Yellow; Black; White] = (0, 0))
    printfn "test3: %b" (validate [Black; Yellow; White; Purple] [Yellow; White; Purple; Black] = (0, 4))
    printfn "test4: %b" (validate [Black; White; Green; Yellow] [Black; White; Red; Purple] = (2, 0))



playGame ()
blackBoxTesting ()
