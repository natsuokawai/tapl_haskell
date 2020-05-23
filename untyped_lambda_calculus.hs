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
termShift c d (TmAbs t1)         = TmAbs (termShift (c + 1) d t1)
termShift c d (TmApp t1 t2)      = TmApp (termShift c d t1) (termShift c d t2)


-- | substitution
-- >>> termSubst 0 (TmVar 1) (TmApp (TmVar 0) (TmAbs (TmAbs (TmVar 2))))
-- TmApp (TmVar 1) (TmAbs (TmAbs (TmVar 3)))
termSubst :: Int -> Term -> Term -> Term
termSubst j s (TmVar k) | k == j    = s
                        | otherwise = TmVar k
termSubst j s (TmAbs t1)            = TmAbs (termSubst (j + 1) (termShift 0 1 s) (t1))
termSubst j s (TmApp t1 t2)         = TmApp (termSubst j s t1) (termSubst j s t2)

-- | beta-reduction rule
-- >>> termSubstTop (TmAbs (TmApp (TmVar 1) (TmApp (TmVar 0) (TmVar 2)))) (TmAbs (TmVar 0))
-- TmApp (TmVar 0) (TmApp (TmAbs (TmVar 0)) (TmVar 1))
termSubstTop :: Term -> Term -> Term
termSubstTop (TmAbs t12) v2 = termShift 0 (-1) (termSubst 0 (termShift 0 1 v2) (t12))

