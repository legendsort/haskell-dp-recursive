{-
    param1 : Double array
    result : number of elements in array which is bigger than average of the array
-}
howManyAboveAverage :: [Double] -> Int
howManyAboveAverage list = length $ filter (> (sum list) / (fromIntegral (length list) )) list

main = do
    print $ howManyAboveAverage [1, 5, 3, 2]
