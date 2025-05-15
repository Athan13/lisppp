module Main where
    import System.Environment

    import Interp (interp)

    main :: IO ()
    main = do
        args <- getArgs
        case args of
            file : tape_size : args_rest -> do
                let debug = "--debug" `elem` args_rest
                let print_chars = "-c" `elem` args_rest
                brainfuck <- readFile file
                result <- interp (read tape_size) brainfuck (debug, print_chars)
                case result of
                    Left e  -> print e
                    Right _ -> putStrLn "\nSuccessfully terminated."
            _ -> putStrLn "Usage:\n\tcabal run exec -- <size-of-tape> <file-name> [-c] [--debug]\
\\n\n\tThe --debug option displays the state of the program at every instruction, and the \
\-c option displays the output of the program as a character rather than a 8-bit unsigned int."
