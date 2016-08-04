import System.Console.Haskeline
import System.Environment
import Text.ParserCombinators.ReadP

data Term = C Char | E Expr

type Expr = [Term]

instance Show Term where
    show (C c) = [c]
    show (E e) = "[" ++ show e ++ "]"
    showList = (++) . concatMap show

instance Read Term where
    readsPrec = const $ readP_to_S readTerm
    readList = readP_to_S readExpr

readTerm :: ReadP Term
readTerm = (C <$> satisfy (`notElem` "[]")) +++ (E <$> between (char '[') (char ']') readExpr)

readExpr :: ReadP Expr
readExpr = many readTerm

data Stack = Expr :-: Stack

initStack :: Stack
initStack = [] :-: initStack
infixr 5 :-:

top :: Int -> Stack -> [Expr]
top n (x :-: s)
    | n <= 0 = []
    | otherwise = x : top (n - 1) s

type Func = Stack -> Stack

evalTerm :: Term -> Func
evalTerm (E e) = (e :-:)
evalTerm (C '^') = dip
evalTerm (C '_') = dup
evalTerm (C '!') = pop
evalTerm (C ':') = cons
evalTerm _ = id

evalExpr :: Expr -> Func
evalExpr = flip . foldl $ flip evalTerm

eval :: Int -> Expr -> [Expr]
eval n = top n . flip evalExpr initStack

dip :: Func
dip (x :-: y :-: s) = y :-: evalExpr x s

dup :: Func
dup (x :-: s) = x :-: x :-: s

pop :: Func
pop (_ :-: s) = s

cons :: Func
cons (x :-: y :-: s) = (E y : x) :-: s

rep :: Int -> String -> String
rep n input = case [x | (x, "") <- reads input] of
    [x] -> unlines . map show $ eval n x
    _ -> error "parse error"

repl :: Int -> InputT IO ()
repl n = do
    minput <- getInputLine "> "
    case minput of
        Nothing -> return ()
        Just input -> do
            catch (outputStrLn $ rep n input) $ \e -> outputStrLn $ show (e :: SomeException)
            repl n

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runInputT defaultSettings $ repl 1
        ["-d", n] -> runInputT defaultSettings . repl $ read n
        (file : _) -> readFile file >>= putStrLn . rep 1
