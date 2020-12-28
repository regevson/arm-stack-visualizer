import Data.Char
import System.Environment (getArgs)

-- STMFD [String] Int
  -- [String] -> registers affecting stack-pointer (are printed into stack visual)
  -- Int -> offset (eventually added/subtracted from stack-pointer)
-- ADD/SUB Int
  -- Int -> offset (eventually added/subtracted from stack-pointer)
data Instruction = STMFD String [String] | -- STMFD cond reglist
                   LDMFD String [String] | -- LDMFD cond reglist
                   ADD String Int String String String | -- ADD cond 'S?' dst op1 op2
                   SUB String Int String String String | -- SUB cond 'S?' dst op1 op2
                   B String String | -- B cond adr
                   BL String String | -- BL cond adr
                   MOV String String String | -- MOV cond dst src
                   LBL String | -- LBL adr
                   NOTRELEVANT -- not affecting stack
                   deriving (Show, Eq)

-- Stack [String] = Stack [frame]
data Stack = Stack [String] deriving Show


main = do
         args <- getArgs
         putStrLn $ "\nStack visual of file: " ++ head args
         content <- readFile $ head args
         let instructions = (map (to_instruction . words) . lines) content
         let rel_inst = filter (\i -> i /= NOTRELEVANT) instructions
         let (hist,banks) = collect rel_inst [] [first_bank]
         let stack = stack_frames (reverse hist) (reverse banks) (Stack [])
         let (Stack finished_stack) = insert_sp stack $ ldreg "r13" $ head banks
         putStrLn ""
         putStrLn $ "              " ++ (head $ lines content) -- analyzed function
         putStrLn "              ┌───────────┐" 
         putStr $ unlines finished_stack
         putStrLn "              └───────────┘" 
         putStrLn "" where
  first_bank = take 16 $ repeat 0 -- init bank


-- convert arm-line to instruction-type
to_instruction :: [String] -> Instruction
to_instruction ("STMFD":dst:src) = STMFD (cln dst) (expand src)
to_instruction ("LDMFD":src:dst) = LDMFD (cln src) (expand dst)
to_instruction (('A':'D':'D':'S':cond):dst:op1:op2:[]) = ADD cond 1 (cln dst) (cln op1) (cln op2)
to_instruction (('A':'D':'D':cond):dst:op1:op2:[]) = ADD cond 0 (cln dst) (cln op1) (cln op2)
to_instruction (('S':'U':'B':'S':cond):dst:op1:op2:[]) = SUB cond 1 (cln dst) (cln op1) (cln op2)
to_instruction (('S':'U':'B':cond):dst:op1:op2:[]) = SUB cond 0 (cln dst) (cln op1) (cln op2)
to_instruction (('B':'L':cond):adr:[]) = BL cond adr
to_instruction (('B':cond):adr:[]) = B cond adr
to_instruction (('M':'O':'V':cond):dst:src:[]) = MOV cond (cln dst) (cln src)
-- to_instruction () = add all supported instructions here
to_instruction (x:xs) = if last x == ':' then LBL (cln x) else NOTRELEVANT

-- clean from special characters -> {,[,:,...
cln str = filter (\c -> isAlpha c || isDigit c) str

-- expand ["r4-r6","lr"] to ["r4","r5","r6","lr"]
expand :: [String] -> [String]
expand xs = concat $ map (extract_reg . (\s -> span (\c -> c /= '-') s)) xs where
  extract_reg (reg, []) = [cln reg] -- if no '-' inside
  extract_reg (from, to) = create_reg_list from to -- if '-' inside
  create_reg_list from to = map ((\r -> 'r':r) . show) [(num from)..(num to)]

-- extract numbers from String
num :: String -> Int
num reg = read $ filter (\c -> isDigit c) reg :: Int

type Bank = [Int] -- list of 16 ints that represent the contents of the 16 registers

-- for every instruction a new bank is created (based on the old one) and added to [Bank]
-- Instructions to analyze -> analyzed Instructions -> created banks -> (anal. Instr., created banks)
collect :: [Instruction] -> [Instruction] -> [Bank] -> ([Instruction],[Bank])
collect [] hist bank = (hist, take len bank) where
  len = (length bank) - 1 -- first added bank is trash

collect (i@(STMFD "sp" reglist):is) hist banks@(b:_) = 
          collect is (i:hist) $ streg "r13" (ldreg "r13" b + length reglist) b : banks

collect (i@(LDMFD "sp" reglist):is) hist banks@(b:_) = 
          collect is (i:hist) $ streg "r13" (ldreg "r13" b - length reglist) b : banks

collect (i@(ADD cond _ dst op1 op2):is) hist banks@(b:_) = if execute cond hist banks -- check cond
                                                           then exec -- if cond is fulfilled
                                                           else collect is (i:hist) (b:banks) where
  exec = collect is (i:hist) $ streg dst (ldreg op1 b + ldreg op2 b) b : banks

collect (i@(SUB cond _ dst op1 op2):is) hist banks@(b:_) = if execute cond hist banks
                                                           then exec
                                                           else collect is (i:hist) (b:banks) where
  exec = collect is (i:hist) $ (streg dst (ldreg op1 b - ldreg op2 b) b):banks

collect (i@(BL cond adr):is) hist banks@(b:_) = if execute cond hist banks
                                                then exec
                                                else collect is (i:hist) (b:banks) where
  exec = collect (unroll ++ is) hist banks
  unroll = (LBL adr) : (reverse $ takeWhile (\i -> i /= (LBL adr)) $ i:hist)

collect (i@(B cond adr):is) hist banks@(b:_) = if execute cond hist banks
                                               then exec
                                               else collect is (i:hist) (b:banks) where
  exec = collect (unroll ++ is) (i:hist) (b:banks)
  unroll = (LBL adr) : (reverse $ takeWhile (\i -> i /= (LBL adr)) $ i:hist)

collect (i@(MOV cond dst src):is) hist banks@(b:_) = if execute cond hist banks
                                                     then exec
                                                     else collect is (i:hist) (b:banks) where
  exec = collect is (i:hist) $ (streg dst (ldreg src b) b):banks

collect (i@(LBL lbl):is) hist banks@(b:_) = collect is (i:hist) (b:banks)

-- collect (i@():is) hist banks@(b:-) = add all supported instructions here


-- change register in a Bank and return new Bank
-- register to set -> value -> Bank -> new Bank
streg :: String -> Int -> Bank -> Bank
streg reg val bank = fst split ++ [val] ++ (tail . snd) split where
  split = splitAt ((num . reg_transl) reg) bank

-- get value of register in a specific Bank
-- if register is immediate -> return immediate
ldreg :: String -> Bank -> Int
ldreg reg_imm bank = case reg_transl reg_imm of
                      ('r':i) -> bank !! (read i :: Int)
                      imm -> read imm :: Int

-- convert the 3 special registers to their 'r:number'-form
reg_transl :: String -> String
reg_transl "sp" = "r13"
reg_transl "pc" = "r14"
reg_transl "lr" = "r15"
reg_transl reg = reg

-- check if condition is fulfilled
-- condition -> analyzed Instructions -> created Banks -> is_fulfilled
execute :: String -> [Instruction] -> [Bank] -> Bool
execute cond history banks = case cond of
                               "" -> True -- no condition
                               "NE" -> rewind history banks /= 0
                               "E" -> rewind history banks == 0
                            -- ... -> add conditions here
                               otherwise -> error $ "condition not supported: " ++ cond

-- search for most recent Instruction with an 'S' and return its computed value
-- analyzed Instructions -> created banks -> relevant value
rewind :: [Instruction] -> [Bank] -> Int
rewind [] [] = error "no supported 'S-instruction' found"
rewind ((ADD _ 1 dst _ _):hist) (b:banks) = ldreg dst b
rewind ((SUB _ 1 dst _ _):hist) (b:banks) = ldreg dst b
-- rewind ((...):hist) (b:banks) = add all Instructions capable of 'S' here
rewind (h:hist) (b:banks) = rewind hist banks -- Instruction without 'S'




-- stack frames onto Stack
-- Instructions (top-to-bot) -> Banks (top-to-bot) -> empty Stack -> built Stack
stack_frames :: [Instruction] -> [Bank] -> Stack -> Stack
stack_frames [] [] stack = stack
stack_frames ((STMFD "sp" reglist):is) (b:bank) (Stack stack) = stack_frames is bank new_stack where
  new_stack = Stack (construct_frames reglist offs stack old_sp)
  offs = new_sp - old_sp
  new_sp = ldreg "r13" b
  old_sp = new_sp - (length reglist)
stack_frames (_:is) (b:bank) stack = stack_frames is bank stack -- no frame added


-- construct frames from @reglist
-- reglist -> frames to add -> stack -> old_sp -> new stack
construct_frames :: [String] -> Int -> [String] -> Int -> [String]
construct_frames reg offs stack sp = (fst sp_split) ++ format reg ++ (drop offs (snd sp_split)) 
  where
  sp_split = splitAt sp stack
  format xs = map format_frame xs
  format_frame frame_content
    | length frame_content < 3 = format_frame $ frame_content ++ " "
    | otherwise = "              |    " ++ frame_content ++ "    |"

-- insert stack-pointer-highlighting into stack
insert_sp :: Stack -> Int -> Stack
insert_sp (Stack stack) sp = Stack (first ++ [head second ++ " » sp (FD)"] ++ tail second) where
  spl = splitAt (sp-1) stack
  first = fst spl
  second = snd spl



