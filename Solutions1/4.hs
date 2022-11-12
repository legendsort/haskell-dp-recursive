import Data.List

-- operator type
data BinOp = Plus | Minus | Times | Div
  deriving (Show, Eq, Enum)

-- expression type
data Expr = Const Int | Op BinOp Expr Expr
  deriving (Show, Eq)

-- evaluate expression
eval :: Expr -> Int
eval exp = case exp of
  Const a -> a
  Op Plus (a) (b) -> (eval a) + (eval b)
  Op Times (a) (b) -> (eval a) * (eval b)
  Op _ (a) (_) -> eval a
  
-- insert element to every element's first array
insertFirst :: Int -> ([Int], [Int]) -> ([Int], [Int])
insertFirst x (a, b) = (x : a, b)

-- insert element to every element's first array in pair array
insertFirstArray :: Int -> [([Int], [Int])] -> [([Int], [Int])]
insertFirstArray _ [] = []
insertFirstArray x [([], t)] = [([x], t)]
insertFirstArray x (y : ys) = (insertFirst x y) : (insertFirstArray x ys)

-- insert element to every element's second array
insertSecond :: Int -> ([Int], [Int]) -> ([Int], [Int])
insertSecond x (a, b) = (a, x : b)

-- insert element to every element's second array in pair array
insertSecondArray :: Int -> [([Int], [Int])] -> [([Int], [Int])]
insertSecondArray _ [] = []
insertSecondArray x [(s, [])] = [(s, [x])]
insertSecondArray x (y : ys) = (insertSecond x y) : (insertSecondArray x ys)

-- split array into two sublists
allSplits :: [Int] -> [([Int], [Int])]
allSplits [] = []
allSplits [x] = []
allSplits (x : xs) = nub ((insertFirstArray x prev) ++ (insertSecondArray x prev) ++ [([x], xs)] ++ [(xs, [x])])
    where prev = allSplits xs

-- join two expressions by inserting operator between them
joinTwoExp :: Expr -> Expr -> [Expr]
joinTwoExp a b = [Op Plus a b, Op Times a b]

-- join two expressions by inserting operator between them
joinTwoExpArray :: ([Expr], [Expr]) -> [Expr]
joinTwoExpArray ([], []) = []
joinTwoExpArray ([], b) = b
joinTwoExpArray (a, []) = a
joinTwoExpArray (xs, ys) = concat [joinTwoExp x y | x <- xs, y <- ys]

-- generate all expressions with 
generateAllExpressions :: [Int] -> [Expr]
generateAllExpressions [] = []
generateAllExpressions [x] = [Const x]
generateAllExpressions x = ans
  where
    split = allSplits x
    ans = concat [joinTwoExpArray (generateAllExpressions a, generateAllExpressions b) | (a, b) <- split] -- split and merge the answers

-- generate all sublist
generateAllSublist :: [Int] -> [[Int]]
generateAllSublist [] = []
generateAllSublist [x] = [[x]]
generateAllSublist (x:xs) = prev ++ [[x]]  ++ [x : a | a <- prev]
  where prev = generateAllSublist xs

-- count down all valid solutions 
countdownAllSolutions :: [Int] -> Int -> [Expr]
countdownAllSolutions [] _ = []
countdownAllSolutions x target = [exp | exp <- expressions, (eval exp) == target]
  where expressions = concat $ map generateAllExpressions $ generateAllSublist x

main = do
    print $ allSplits [1, 1, 2]
    print $ countdownAllSolutions [3, 2, 5, 8, 4] 5
