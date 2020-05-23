main :: IO ()
main = return ()

-- | type of term
data Term = TmVar Int
          | TmAbs Term
          | TmApp Term Term
          deriving Show


-- | shift
-- >>> termShift 0 2 (TmAbs (TmAbs (TmApp (TmVar 1) (TmApp (TmVar 0) (TmVar 2)))))
-- TmAbs (TmAbs (TmApp (TmVar 1) (TmApp (TmVar 0) (TmVar 4))))
termShift :: Int -> Int -> Term -> Term
termShift c d (TmVar k) | k < c  = TmVar k
                        | k >= c = TmVar (k + d)
termShift c d (TmAbs t)          = TmAbs (termShift (c + 1) d t)
termShift c d (TmApp t1 t2)      = TmApp (termShift c d t1) (termShift c d t2)


