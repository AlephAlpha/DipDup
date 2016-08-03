import System.Console.Haskeline
import Text.ParserCombinators.ReadP

data Term = C Char | E Expr

type Expr = [Term]
type Stack = [Expr]
type Func = Stack -> Stack

-- instance Monoid Func where
--     mempty = id
--     mappend = flip (.)

instance Show Term where
    show (C c) = [c]
    show (E e) = "[" ++ show e ++ "]"
    showList = (++) . concatMap show

instance Read Term where
    readsPrec = const $ readP_to_S readTerm
    readList = readP_to_S readExpr

readTerm :: ReadP Term
readTerm = (C <$> satisfy (flip notElem "[]")) +++ (E <$> between (char '[') (char ']') readExpr)

readExpr :: ReadP Expr
readExpr = many readTerm

evalTerm :: Term -> Func
evalTerm (E e) = (e :)
evalTerm (C '^') = dip
evalTerm (C '_') = dup
evalTerm (C '!') = pop
evalTerm (C ':') = cons
evalTerm _ = id

evalExpr :: Expr -> Func
evalExpr = flip . foldl $ flip evalTerm

eval :: Expr -> Expr
eval = head . flip evalExpr (repeat [])

dip (x : y : s) = y : evalExpr x s
dup (x : s) = x : x : s
pop (x : s) = s
cons (x : y : s) = (E y : x) : s

readEither :: Read a => String -> Either String a
readEither s = case [x | (x, "") <- reads s] of
    [x] -> Right x
    [] -> Left "no parse"
    _ -> Left "ambigous parse"

repl :: InputT IO ()
repl = do
    minput <- getInputLine "> "
    case minput of
        Nothing -> return ()
        Just input -> do
            case readEither input of
                Left s -> outputStrLn s
                Right e -> outputStrLn . show $ eval e
            repl

main :: IO ()
main = runInputT defaultSettings repl
