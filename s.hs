-- The following piece of code aims to implement the function
-- string2Expr :: String -> Expr mentioned in the coursework, at question 3.a).
-- Feel free to use that code to check whether your answer to question 3.a) is
-- correct or not. You are also free to reuse part of it in your submission if
-- you find that useful.

-- I do not claim that the code below is particularly elegant and it is
-- certainly not documented. But to the best of my knowledge, it should do the
-- job.

-- It is written using only material we have covered in the lecture. But if you
-- have ever a need to parse non-trivial expessions in the future (typically
-- anything that requires to check for proper bracketing), I would suggest
-- not using the same approach (which is quite painful as you can see), but
-- dedicated parsing tools and libraries. In the Haskell ecosystem there are
-- two popular approaches (afaik): parser generators (one is called _happy_) and
-- parser combinators libraries such as _Parsec_.


import Data.List
import Data.Char
import Data.Maybe

data BinOp = Plus | Minus | Times | Div
  deriving (Show, Eq, Enum)

data Expr = Const Int | Op BinOp Expr Expr
  deriving (Show, Eq)

opsCharList :: [(BinOp, Char)]
opsCharList = [(Plus, '+'),(Minus, '-'),(Times, '*'),(Div, '/')]

orderFromList :: Eq a => [a] -> a -> a -> Ordering
orderFromList xs x y = ordMaybePartial (elemIndex x xs) (elemIndex y xs)
  where ordMaybePartial :: Ord a => Maybe a -> Maybe a -> Ordering
        ordMaybePartial (Just x) (Just y) = compare x y
        ordMaybePartial _        _        = EQ

isOp :: Char -> Bool
isOp = (`elem` map snd opsCharList) 

isOpStr :: [Char] -> Bool
isOpStr [x] = isOp x
isOpStr _   = False

cmpOp :: BinOp -> BinOp -> Ordering
cmpOp = orderFromList (map fst opsCharList)

char2Op :: Char -> BinOp
char2Op d = fst pair
  where Just pair = find (\(_,c) -> c == d) opsCharList

op2Char :: BinOp -> Char
op2Char d = snd pair
  where Just pair = find (\(c,_) -> c == d) opsCharList

splitAtParensL :: Int -> String -> (String, [String])
splitAtParensL 0 (d : s) | isDigit d = (d : sl, uncurry (:) (splitAtParensL 0 sr))
                         | isOp    d = ([d], uncurry (:) (splitAtParensL 0 s))
  where (sl, sr) = span isDigit s
splitAtParensL 1 (')' : s) = (")", uncurry (:) (splitAtParensL 0 s))
splitAtParensL n (d   : s) = (d : sl, sr)
  where (sl, sr) = splitAtParensL (n + i) s
        i        = case d of
                    '(' -> 1
                    ')' -> (-1)
                    _   -> 0
splitAtParensL _ [] = ([], [])

splitAtParens :: String -> [String]
splitAtParens = filter (not . null) . uncurry (:) . splitAtParensL 0


splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn x (y : ys) | x == y    = ([], ys)
                   | otherwise = (y : ls, rs)
                        where (ls, rs) = splitOn x ys

rSplitOn :: Eq a => a -> [a] -> ([a], [a])
rSplitOn x = swap . mapHPair reverse . splitOn x . reverse
  where mapHPair f (x, y) = (f x, f y)
        swap (x, y)       = (y, x)

hasSurroundingParens :: String -> Bool
hasSurroundingParens s = decompParens (splitAtParens s)
  where decompParens ['(' : sr] = last sr == ')'
        decompParens _          = False


splitAtMainOp :: [Char] -> Maybe (String, BinOp, String)
splitAtMainOp s
                | any isOpStr decomp && length s >= 2 = Just (sl, mainOp, sr)
                      where (sl', sr')     = rSplitOn [op2Char mainOp] decomp
                            decomp         = splitAtParens s
                            sl             = concat sl'
                            sr             = concat sr'
                            mainOp         = minimumBy cmpOp listOps
                            listOps        = map (char2Op . head) (filter isOpStr decomp)
splitAtMainOp _ = Nothing

string2Expr :: [Char] -> Maybe Expr
string2Expr = nstring2Expr . filter (/= ' ')
   where nstring2Expr :: String -> Maybe Expr
         nstring2Expr s | all isDigit s          = (Just . Const . read) s
                        | hasSurroundingParens s = (nstring2Expr . init . tail) s
                        | isSplittable s         =  case (nstring2Expr sl, nstring2Expr sr) of
                                                      (Just rl, Just rr) -> Just (Op op rl rr)
                                                      _                  -> Nothing
                              where isSplittable s = (not . null . splitAtMainOp) s
                                    (sl, op, sr)   = fromJust (splitAtMainOp s)


main = do
  print "hh"