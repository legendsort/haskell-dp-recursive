-- operator type
data BinOp = Plus  | Times | Minus | Div
  deriving (Show, Eq, Enum)

-- expression type
data Expr = Const Int | Op BinOp Expr Expr
  deriving (Show, Eq)

-- convert String Operator to real operator
convert2Operator :: BinOp -> String
convert2Operator Plus = "+"
convert2Operator Times = "*"
convert2Operator Minus = "-"
convert2Operator Div = "/"

-- convert expression to string
expr2String :: Expr -> String
expr2String exp = case exp of 
  Const a -> show a -- when it is simple value
  Op oper (a) (b) -> "(" ++ (expr2String a) ++ " " ++ (convert2Operator oper) ++ " " ++ (expr2String b) ++ ")" -- then can divide it in two

-- convert left-balanced experssion
rewriteAssoc :: Expr -> Expr
rewriteAssoc exp = case exp of
  Op Plus (a) (Op Plus (b) (c)) -> Op Plus (Op Plus (rewriteAssoc a) (rewriteAssoc b)) (rewriteAssoc c) -- change balance
  Op Times (a) (Op Times (b) (c)) -> Op Times (Op Times (rewriteAssoc a) (rewriteAssoc b)) (rewriteAssoc c) -- change balance
  Op Plus (a) (b) -> Op Plus (rewriteAssoc a) (rewriteAssoc b)
  Op Times (a) (b) -> Op Times (rewriteAssoc a) (rewriteAssoc b)
  exp -> exp

main = do
  let a = rewriteAssoc (Op Plus (Const 2) (Op Div (Const 3) (Const 4)))
  print a
  print $ expr2String a