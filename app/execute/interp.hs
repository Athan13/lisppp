module Interp where
    import qualified Data.Vector as V
    import qualified Data.Vector.Mutable as MV

    import Data.Word (Word8)
    import Data.Maybe (mapMaybe)
    import Control.Monad.Trans.Except ( ExceptT, throwE, runExceptT )
    import Control.Monad.Trans.State  ( StateT(runStateT), modify, get, put )
    import Control.Monad.IO.Class (liftIO)
    import Control.Monad.Trans.Class (lift)
    import Control.Monad (forever)

    import System.IO
    import Text.Read (readMaybe)

    type BFTape = V.Vector Word8
    data BFInstruction = BFPlus | BFMinus | BFRight | BFLeft | BFLoopL | BFLoopR | BFPoint | BFComma
    type BFProgram = V.Vector BFInstruction

    instance Show BFInstruction where
        show i = case i of {
            BFPlus -> "("; BFMinus -> ")"; 
            BFLeft -> "<"; BFRight -> ">";
            BFLoopL -> "["; BFLoopR -> "]";
            BFPoint -> "{"; BFComma -> "}";
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

    
    -- Program state is either a tuple (tape, head position, loop left, instruction number), 
    -- or the position of the first error
    data InterpError = InterpError Int String BFInstruction
    instance Show InterpError where 
        show (InterpError i s instr) = concat [s, " on character ", show instr, " at position ", show i]

    type ProgramState = (BFTape, Int, BFProgram, Int, [Int])
    type Program = ExceptT InterpError (StateT ProgramState IO)
        
    execInstr :: BFInstruction -> Program ()
    execInstr BFPlus = lift $ modify incrementAtHead
        where incrementAtHead (tape, pos, instr, i_pos, l) = ((V.modify (\v -> MV.modify v (+1) pos) tape), pos, instr, i_pos + 1, l)
    execInstr BFMinus = lift $ modify decrementAtHead
        where decrementAtHead (tape, pos, instr, i_pos, l) = ((V.modify (\v -> MV.modify v (+255) pos) tape), pos, instr, i_pos + 1, l)
    execInstr BFLeft = lift $ modify shiftLeft
        where shiftLeft (tape, pos, instr, i_pos, l) = (tape, (pos - 1) `mod` V.length tape, instr, i_pos + 1, l)
    execInstr BFRight = lift $ modify shiftRight
        where shiftRight (tape, pos, instr, i_pos, l) = (tape, (pos + 1) `mod` V.length tape, instr, i_pos + 1, l)
    execInstr BFLoopL = lift $ modify beginLoop
        where beginLoop (tape, pos, instr, i_pos, l) = (tape, pos, instr, i_pos + 1, i_pos : l)
    execInstr BFLoopR = do
        (tape, pos, instr, i_pos, l) <- lift get
        case l of
            []   -> throwE (InterpError i_pos "Invalid loop" BFLoopR)
            s@(l:ls) -> if (tape V.! pos == 0) 
                            then (lift $ put (tape, pos, instr, i_pos + 1, ls))
                            else (lift $ put (tape, pos, instr, l, s))
    execInstr BFPoint = do
        (tape, pos, instr, i_pos, l) <- lift get
        liftIO $ putStr $ (show (tape V.! pos) ++ " ")
        lift $ put (tape, pos, instr, i_pos + 1, l)
    execInstr BFComma = do
        (tape, pos, instr, i_pos, l) <- lift get
        liftIO $ putStr "\nPass in value (0-255): "
        liftIO $ hFlush stdout
        num <- liftIO $ getLine
        case readMaybe num of
            Just num -> lift $ put (V.modify (\v -> MV.write v pos (fromIntegral num)) tape, pos, instr, i_pos + 1, l)
            Nothing -> throwE $ InterpError i_pos ("Invalid read: required number from 0 to 255, got " ++ num ++ " insetad") BFComma

    execProgram :: Program ()
    execProgram = do
        p@(tape, pos, instrs, i_pos, l) <- lift $ get
        case instrs V.!? i_pos of
            Just instr -> do {execInstr instr; execProgram}
            Nothing    -> return ()
            
    interp :: Int -> String -> IO (Either InterpError ())
    interp tape_size s = do
        (result, _) <- runStateT (runExceptT execProgram) startState
        return result
            where 
                instructions = toInstr s
                startState = ((V.replicate tape_size 0), 0, V.fromList instructions, 0, []) :: ProgramState
    