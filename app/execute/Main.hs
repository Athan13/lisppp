module Main where
    import System.Environment

    import Interp (interp)

    main :: IO ()
    main = do
        args <- getArgs
        case args of
            tape_size : file : _ -> do
                brainfuck <- readFile file
                result <- interp (read tape_size) brainfuck
                case result of
                    Left e  -> print e
                    Right _ -> putStrLn "\nSuccessfully terminated."
            _ -> putStrLn "Usage:\n\tcabal run exec -- <size-of-tape> <file-name>"
