
import qualified Data.Map as Map
import qualified Control.Monad as Mon


data Term f s = Const s | Call f [Term f s] deriving (Eq, Show)

--extends a Set of function symbols or constant symbols to also include 
--  matching variable symbols
data PatVar a = Pat Int | Lit a deriving (Show)

eval :: (f -> [s] -> s) -> (Term f s) -> s
eval v (Const t)   = t
eval v (Call f ts) = v f (map (eval v) ts)

match :: (Eq f, Eq s) => (Term (PatVar f) (PatVar s)) -> (Term f s) -> (Maybe (Map.Map Int (Term f s)))
match (Call (Lit f) ts) (Call g rs) 
    | f /= g                 = Nothing
    | length ts /= length rs = Nothing
    | otherwise              = undefined
match (Call (Pat v) ts) (Call g rs) = undefined



data RealOps = Sum | Product | Negate | Invert deriving (Eq, Show)

evalReal :: (Fractional s) => RealOps -> [s] -> s
evalReal Sum     cs = sum     cs
evalReal Product cs = product cs
evalReal Negate [c] = -c
evalReal Invert [c] = 1 / c