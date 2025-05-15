module Interp where
    import qualified Data.Vector as V
    import qualified Data.Vector.Mutable as MV

    import Data.Word (Word8)
    import Data.Char (chr)
    import Data.Maybe (mapMaybe)
    import Control.Monad.Trans.Except ( ExceptT, throwE, runExceptT )
    import Control.Monad.Trans.State  ( StateT(runStateT), modify, get, put )
    import Control.Monad.IO.Class (liftIO)
    import Control.Monad.Trans.Class (lift)

    import System.IO
    import Text.Read (readMaybe)

    type BFTape = V.Vector Word8
    data BFInstruction = BFPlus | BFMinus | BFRight | BFLeft | BFLoopL | BFLoopR | BFPoint | BFComma
        deriving Eq
    type BFSource = V.Vector BFInstruction

    instance Show BFInstruction where
        show i = case i of {
            BFPlus -> "(";   BFMinus -> ")"; 
            BFLeft -> "<";   BFRight -> ">";
            BFLoopL -> "[";  BFLoopR -> "]";
            BFPoint -> "{";  BFComma -> "}";
        }

    toInstr :: [Char] -> [BFInstruction]
    toInstr = mapMaybe charToInstr
        where
            charToInstr :: Char -> Maybe BFInstruction
            charToInstr c = case c of {
                '(' -> Just BFPlus;   ')' -> Just BFMinus;
                '<' -> Just BFLeft;   '>' -> Just BFRight;
                '[' -> Just BFLoopL;  ']' -> Just BFLoopR;
                '{' -> Just BFPoint;  '}' -> Just BFComma;
                _ -> Nothing
            }

    
    -- Program state is either a tuple (tape, head position, instructions, instruction number), 
    -- or the position of the first error
    data InterpError = InterpError Int String BFInstruction
    instance Show InterpError where 
        show (InterpError i s instr) = concat [s, " on character ", show instr, " at position ", show i]

    type BFProgramState = (BFTape, Int, BFSource, Int)
    type BFProgram = ExceptT InterpError (StateT BFProgramState IO)
        
    execInstr :: BFInstruction -> Bool -> BFProgram ()
    execInstr BFPlus _ = lift $ modify incrementAtHead
        where incrementAtHead (tape, pos, instr, i_pos) = ((V.modify (\v -> MV.modify v (+1) pos) tape), pos, instr, i_pos + 1)
    execInstr BFMinus _ = lift $ modify decrementAtHead
        where decrementAtHead (tape, pos, instr, i_pos) = ((V.modify (\v -> MV.modify v (+255) pos) tape), pos, instr, i_pos + 1)
    execInstr BFLeft _ = lift $ modify shiftLeft
        where shiftLeft (tape, pos, instr, i_pos) = (tape, (pos - 1) `mod` V.length tape, instr, i_pos + 1)
    execInstr BFRight _ = lift $ modify shiftRight
        where shiftRight (tape, pos, instr, i_pos) = (tape, (pos + 1) `mod` V.length tape, instr, i_pos + 1)
    execInstr BFLoopL _ = do
        (tape, pos, instr, i_pos) <- lift get
        if (tape V.! pos == 0) 
            then case scanRight instr (i_pos + 1) 1 of
                Just i_pos' -> lift $ put (tape, pos, instr, i_pos')
                Nothing -> throwE (InterpError i_pos "Invalid loop: no right match found" BFLoopL)
            else (lift $ put (tape, pos, instr, i_pos + 1))
        where
            scanRight vec i ord = case vec V.!? i of
                Just curr_i -> 
                    if curr_i == BFLoopR && ord <= 1
                        then Just i
                    else if curr_i == BFLoopR
                        then scanRight vec (i + 1) (ord - 1)
                    else if curr_i == BFLoopL
                        then scanRight vec (i + 1) (ord + 1)
                    else scanRight vec (i + 1) ord
                Nothing -> Nothing
                    
    execInstr BFLoopR _ = do
        (tape, pos, instr, i_pos) <- lift get
        if tape V.! pos /= 0 
            then case scanLeft instr (i_pos - 1) 1 of
                Just i_pos' -> lift $ put (tape, pos, instr, i_pos')
                Nothing -> throwE (InterpError i_pos "Invalid loop: no left match found" BFLoopR)
            else (lift $ put (tape, pos, instr, i_pos + 1))
        where
            scanLeft vec i ord = case vec V.!? i of
                Just curr_i -> 
                    if curr_i == BFLoopL && ord <= 1
                        then Just i
                    else if curr_i == BFLoopL
                        then scanLeft vec (i - 1) (ord - 1)
                    else if curr_i == BFLoopR
                        then scanLeft vec (i - 1) (ord + 1)
                    else scanLeft vec (i - 1) ord
                Nothing -> Nothing
    execInstr BFPoint print_chars = do
        (tape, pos, instr, i_pos) <- lift get
        if print_chars then do
            let c = chr $ fromIntegral $ tape V.! pos 
            liftIO $ putStr (show c ++ " ")
            liftIO $ hFlush stdout
            lift $ put (tape, pos, instr, i_pos + 1)
        else do 
            let c = tape V.! pos
            liftIO $ putStr (show c ++ " ")
            liftIO $ hFlush stdout
            lift $ put (tape, pos, instr, i_pos + 1)
    execInstr BFComma print_chars = do
        (tape, pos, instr, i_pos) <- lift get
        liftIO $ putStr "\nPass in value (0-255): "
        liftIO $ hFlush stdout
        num <- liftIO $ getLine
        case readMaybe num of
            Just num -> lift $ put (V.modify (\v -> MV.write v pos (fromIntegral num)) tape, pos, instr, i_pos + 1)
            Nothing -> throwE $ InterpError i_pos ("Invalid read: required number from 0 to 255, got " ++ num ++ " insetad") BFComma

    execProgram :: (Bool, Bool) -> BFProgram ()
    execProgram args@(debug, print_chars) = do
        p@(tape, pos, instrs, i_pos) <- lift $ get
        case instrs V.!? i_pos of
            Just instr -> do
                if debug then liftIO $ print ((V.take 10 tape), pos, i_pos) else return ()
                execInstr instr print_chars
                execProgram args
            Nothing -> return ()
            
    interp :: Int -> String -> (Bool, Bool) -> IO (Either InterpError ())
    interp tape_size s args = do
        (result, _) <- runStateT (runExceptT (execProgram args)) startState
        return result
            where 
                instructions = toInstr s
                startState = ((V.replicate tape_size 0), 0, V.fromList instructions, 0) :: BFProgramState
    