

-- get the average of the double array
average :: [Double] -> Double
average arr = (sum arr) / (fromIntegral (length arr))

-- get the number of averages 
howManyAboveAverage :: [Double] -> Int
howManyAboveAverage [] = 0
howManyAboveAverage arr = length $ filter (> avg) arr -- get the array whose element is bigger thant average and return the length
    where avg = average arr

main = do
    -- result is 2
    print $ howManyAboveAverage [2, 3, 5, 5]
    -- result is 2
    print $ howManyAboveAverage [1, 2, 3, 4, 5]
    -- result is 0
    print $ howManyAboveAverage []
    -- result is 3
    print $ howManyAboveAverage [1, 1.5, 2.3, 7.3, 9.2, 19]
    -- result is 0
    print $ howManyAboveAverage [3.3, 3.3, 3.3, 3.3]
