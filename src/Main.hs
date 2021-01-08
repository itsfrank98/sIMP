module Main where
import Interpreter(programExec, OutputEnv(..))
import Parser (parse, getParsedCommands, parseFailed)

main :: IO()
main = do
putStrLn "Welcome to...                "
putStrLn "      _    __    __   ____   "
putStrLn "     | |  | \\\\  // | | D__)  "
putStrLn " ___ | |  | |\\\\//| | | |     "
putStrLn "(__  | |  | |    | | | |     "
putStrLn "___) |_|  |_|    |_| |_|     "
putStrLn "                             "
putStrLn "Insert the path to the file you want to use!"
input <- getLine;
p <- readFile input
let c = parse p
case (parseFailed c) of
    Nothing -> do
        let s = programExec (getParsedCommands c) []
        putStrLn "\nInput Program\n"
        putStrLn p
        --putStrLn "\nRepresentation of the program:\n"
        --print (getParsedCommands c)
        putStrLn "\n"
        case s of 
            ResultEnv _ -> do
                putStrLn "EXECUTION SUCCEDED!!"
                putStrLn "\nState of the memory:\n"
                putStrLn "        NAME     TYPE           VALUE"
                print s
            ErrorEnv _ -> do
                print s
    (Just code) -> do
        putStrLn "\nParsing failed. These commands weren't parsed\n"
        print code 