
import qualified Data.Map as Map
import qualified Control.Monad as Mon


data Term f c = Const c | Call f [Term f c] deriving (Eq, Show)

--extends a Set of function symbols or constant symbols to also include 
--  matching variable symbols
data PatVar a = Pat Int | Lit a deriving (Show)

eval :: (f -> [y] -> y) -> (c -> y) -> (Term f c) -> y
eval u v (Const t)   = v t
eval u v (Call f ts) = u f (map (eval u v) ts)

match :: (Eq f, Eq c) => (Term f (PatVar c)) -> (Term f c) -> (Maybe (Map.Map Int (Term f c)))
match (Call f ts) (Call g rs) 
    | f /= g                 = Nothing
    | length ts /= length rs = Nothing
    | otherwise              = undefined



data RealOps = Sum | Product | Negate | Invert deriving (Eq, Show)

evalReal :: (Fractional c) => RealOps -> [c] -> c
evalReal Sum     cs = sum     cs
evalReal Product cs = product cs
evalReal Negate [c] = -c
evalReal Invert [c] = 1 / c

