import Data.Char
import System.Environment (getArgs)

-- STMFD [String] Int
  -- [String] -> registers affecting stack-pointer (are printed into stack visual)
  -- Int -> offset (eventually added/subtracted from stack-pointer)
-- ADD/SUB Int
  -- Int -> offset (eventually added/subtracted from stack-pointer)
data Instruction = STMFD [String] Int | LDMFD Int | ADD Int | SUB Int | STACKNOTINVOLVED
                     deriving (Show, Eq)

data Stack = Stack [String] Int deriving Show


main = do
         args <- getArgs
         content <- readFile $ head args
         let output = (create_instructions . map words . lines) content
         let (Stack stack sp) = fill_stack output (Stack [] 0)
         putStrLn $ "\nStack visual of file: " ++ head args ++ "\n"
         --print $ show sp
         putStrLn "               ___________" 
         putStr $ unlines stack
         putStrLn ""


-- convert list of raw instructions into list of 'Instruction'-types
create_instructions :: [[String]] -> [Instruction]
create_instructions [] = error "empty program"
create_instructions xs = filter (\ys -> ys /= STACKNOTINVOLVED) $ map (\ys -> to_instruction ys) xs

-- convert raw instruction (with stack involvement) into 'Instruction'-type
to_instruction :: [String] -> Instruction
to_instruction ("STMFD":"sp!,":reg) = STMFD (fst $ offset reg) (snd $ offset reg)
to_instruction ("LDMFD":"sp!,":reg) = LDMFD (snd $ offset reg)
to_instruction ("ADD":"sp,":"sp,":reg:[]) = ADD (snd $ offset [reg])
to_instruction ("SUB":"sp,":"sp,":reg:[]) = SUB (snd $ offset [reg])
to_instruction instruction = STACKNOTINVOLVED


{- 3 cases:
 - 1) #4
 - 2) r12 or lr
 - 3) r4-r12
-}

-- calc offset to stack resulting from values in @list
offset :: [String] -> ([String], Int)
offset list = foldl1 fol tuple_list where
  tuple_list = map (form_tuple . (\s -> span (\c -> c /= '-') s)) list
  fol = (\tuple1 -> (\tuple2 -> (fst tuple1 ++ fst tuple2, snd tuple1 + snd tuple2)))

-- form tuple ([String], Int) with [String] being the registers contributing to the offset
-- and Int being the offset
form_tuple :: (String, String) -> ([String], Int)
form_tuple (('#':imm),[]) = ([], read imm :: Int) -- 1)
form_tuple (reg,[]) = ([filter (\c -> isAlpha c || isDigit c) reg], 1)  -- 2)
form_tuple (from,to) = (reg_list, length reg_list) where -- 3)
  reg_list = map ((\r -> 'r':r) . show) [(isolate_num from)..(isolate_num to)]
  isolate_num str = read $ filter (\c -> isDigit c) str :: Int



-- fill stack with Instructions
fill_stack :: [Instruction] -> Stack -> Stack
fill_stack [] stack = insert_sp stack
fill_stack (x:xs) stack = fill_stack xs (draw_frames x stack)

-- Depending on the instruction new frame(s) on stack is (are) created + adjust stack-pointer
draw_frames :: Instruction -> Stack -> Stack
draw_frames (STMFD reg offs) (Stack stack sp) = Stack (place_frames reg offs stack sp) (sp+offs)
draw_frames (LDMFD offs) (Stack stack sp) = Stack stack (sp-offs)
draw_frames (ADD offs) (Stack stack sp) = Stack stack (sp+offs)
draw_frames (SUB offs) (Stack stack sp) = Stack stack (sp-offs)

-- insert registers (@reg) into the stack after stack-pointer
place_frames :: [String] -> Int -> [String] -> Int -> [String]
place_frames reg offs stack sp = (fst sp_split) ++ format_frames reg ++ (drop offs (snd sp_split)) 
  where
  sp_split = splitAt sp stack
  format_frames xs = map format_frame xs
  format_frame frame_content
    | length frame_content < 3 = format_frame $ frame_content ++ " "
    | otherwise = "              |    " ++ frame_content ++ "    |"

-- insert stack-pointer-highlighting into stack
insert_sp :: Stack -> Stack
insert_sp (Stack stack sp) = Stack (first ++ [head second ++ " sp"] ++ tail second) sp where
  spl = splitAt (sp-1) stack
  first = fst spl
  second = snd spl

