import Data.List

data BinOp = Plus | Minus | Times | Div
  deriving (Show, Eq, Enum)

data Expr = Const Int | Op BinOp Expr Expr
  deriving (Show, Eq)
  
{-
    param1 : inject number
    param2 : split array
    result : inject number to every first of split array
-}
injectX :: Int -> [([Int], [Int])] -> [([Int], [Int])]
injectX _ [] = []
injectX x [([], t)] = [([x], t)]
injectX x ((first, second) : ys) = (x : first, second) : (injectX x ys)

{-
    param1 : inject number
    param2 : split array
    result : inject number to every first of split array
-}
injectY :: Int -> [([Int], [Int])] -> [([Int], [Int])]
injectY _ [] = []
injectY x [(s, [])] = [(s, [x])]
injectY x ((first, second) : ys) = (first, x : second) : (injectY x ys)

{-
    param1 : input array
    result : enumerate all possible ways of splitting a list of elements in two distinct sublists containing the same elements
-}
allSplits :: [Int] -> [([Int], [Int])]
allSplits [] = []
allSplits [_] = []
allSplits (x : xs) = nub ans
    where
        prev = allSplits xs
        ans =  [([x], xs)] ++ [(xs, [x])] ++ (injectX x prev) ++ (injectY x prev)

{-
    param1 : input array
    result : get all the subset of array
-}
subset :: [Int] -> [[Int]]
subset [] = []
subset [x] = [[x]]
subset (x:xs) = ans
  where
    prev = subset xs
    ans = prev  ++ [x : a | a <- prev] ++ [[x]] 
{-
    param1 : pair of two expression arrays
    result : enumerate all possible ways of splitting a list of elements in two distinct sublists containing the same elements
-}
merge :: ([Expr], [Expr]) -> [Expr]
merge ([], []) = []
merge ([], b) = b
merge (a, []) = a
merge (xs, ys) = concat [[Op Plus x y, Op Times x y] | x <- xs, y <- ys]

{-
    param1 : input array
    result : generate all possible expressions
-}
generate :: [Int] -> [Expr]
generate [] = []
generate [x] = [Const x]
generate x = ans
  where
    split = allSplits x
    ans = concat [merge (generate a, generate b) | (a, b) <- split] 

{-
    param1 : input expression
    result : evaluate the expression
-}
evaluate :: Expr -> Int
evaluate e = case e of
  Const a -> a
  Op Plus (a) (b) -> (evaluate a) + (evaluate b)
  Op Times (a) (b) -> (evaluate a) * (evaluate b)
  Op _ (a) (_) -> evaluate a

{-
    param1 : input array
    param2 : target value for the problem
    result : lists all of the arithmetical expression that a contestant could use to compute the target from the given number
-}
countdownAllSolutions :: [Int] -> Int -> [Expr]
countdownAllSolutions [] _ = []
countdownAllSolutions x target = ans
  where
    expressionList = map generate $ subset x
    generateExpressions = concat expressionList
    ans = [e | e <- generateExpressions, (evaluate e) == target]

main = do
    print $ allSplits [1, 1, 2]
    print $ countdownAllSolutions [7, 4, 3, 2, 6] 8
