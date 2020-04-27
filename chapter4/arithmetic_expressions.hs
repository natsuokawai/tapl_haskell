-- type declaration
data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving (Show)

-- check whether a term is a numeric value
isNumericVal :: Term -> Bool
isNumericVal TmZero      = True
isNumericVal (TmSucc t1) = isNumericVal t1
isNumericVal _           = False

-- check whether a term is a value
isVal :: Term -> Bool
isVal TmTrue               = True
isVal TmFalse              = True
isVal t1 | isNumericVal t1 = True
isVal _                    = False

-- single-step evaluator 
eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue t2 t3)     = Just t2
eval1 (TmIf TmFalse t2 t3)    = Just t3
eval1 (TmIf t1 t2 t3)         = case eval1 t1 of
                                  Just t1' -> Just ( TmIf t1' t2 t3)
                                  Nothing  -> Nothing
eval1 (TmSucc t1)             = case eval1 t1 of
                                  Just t1' -> Just (TmSucc t1')
                                  Nothing  -> Nothing
eval1 (TmPred TmZero)         = Just TmZero
eval1 (TmPred (TmSucc nv1))
    | isNumericVal nv1        = Just nv1
eval1 (TmPred t1)             = case eval1 t1 of
                                  Just t1' -> Just (TmPred t1')
                                  Nothing  -> Nothing
eval1 (TmIsZero TmZero)       = Just TmTrue
eval1 (TmIsZero (TmSucc nv1))
    | isNumericVal nv1        = Just TmFalse
eval1 (TmIsZero t1)           = case eval1 t1 of
                                  Just t1' -> Just (TmIsZero t1')
                                  Nothing  -> Nothing
eval1 _                       = Nothing

eval :: Term -> Maybe Term
eval t = case eval1 t of
           Just t' -> eval1 t'
           Nothing -> Just t

-- big-step evaluator
eval' :: Term -> Maybe Term
eval' v | isVal v     = Just v
eval' (TmIf t1 t2 t3) = case eval' t1 of
                          Just TmTrue                   -> eval' t2
                          Just TmFalse                  -> eval' t3
eval' (TmSucc t1)     = case eval' t1 of
                          Just nv1 | isVal nv1          -> Just (TmSucc nv1)
eval' (TmPred t1)     = case eval' t1 of
                          Just TmZero                   -> Just TmZero
                          Just (TmSucc nv1) | isVal nv1 -> Just nv1
eval' (TmIsZero t1)   = case eval' t1 of
                          Just TmZero                   -> Just TmTrue
                          Just (TmSucc nv1) | isVal nv1 -> Just TmFalse
eval' _               = Nothing

