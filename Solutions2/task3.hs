import Data.List

data BinOp = Plus | Minus | Times | Div 
  deriving (Show, Eq, Enum)

data Expr = Const Int | Op BinOp Expr Expr
  deriving (Show, Eq)

{-
    make pair with BinOp and Operator
-}
opsCharList :: [(BinOp, String)]
opsCharList = [(Plus, "+"),(Minus, "-"),(Times, "*"),(Div, "/")]

{-
    define the order of operator
-}
cmpOp :: BinOp -> BinOp -> Ordering
cmpOp = orderFromList (map fst opsCharList)

{-
    order from list
-}
orderFromList :: Eq a => [a] -> a -> a -> Ordering
orderFromList xs x y = ordMaybePartial (elemIndex x xs) (elemIndex y xs)
  where ordMaybePartial :: Ord a => Maybe a -> Maybe a -> Ordering
        ordMaybePartial (Just x) (Just y) = compare x y
        ordMaybePartial _        _        = EQ

{-
    convert BinOp to operator
-}
op2Char :: BinOp -> String
op2Char d = snd pair
  where Just pair = find (\(c,_) -> c == d) opsCharList

{-
    for the problem
    convert expression to string
-}
expr2String :: Expr -> String
expr2String (Const x) = show x
expr2String (Op op x y) = "("++(expr2String x)++" "++(op2Char op)++" "++(expr2String y) ++")"

{-
    for the problem
    turns every expression into an equivalent left-balanced one with the same constants
-}
rewriteAssoc :: Expr -> Expr
rewriteAssoc (Op Plus (x) (Op Plus (y) (z))) = Op Plus (Op Plus (rewriteAssoc x) (rewriteAssoc y)) (rewriteAssoc z) 
rewriteAssoc (Op Plus (x) (y)) = Op Plus (rewriteAssoc x) (rewriteAssoc y)
rewriteAssoc (Op Times (x) (Op Times (y) (z))) = Op Times (Op Times (rewriteAssoc x) (rewriteAssoc y)) (rewriteAssoc z) 
rewriteAssoc (Op Times (x) (y)) = Op Times (rewriteAssoc x) (rewriteAssoc y)
rewriteAssoc x = x

main = do
  print $ expr2String $ rewriteAssoc (Op Div (Op Plus (Const 4) (Op Times (Const 2) (Const 33))) (Const 2))