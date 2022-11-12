
{-
    param1 : float number
    result : cut float number with 2 digits under point
-}
pick :: Float -> Float
pick x = (fromIntegral (floor (x * 100))) / 100

{-
    param1 : diameter of pizza
    param2 : number of topping
    param3 : number of sauce
    param4 : distance for delivery
    result : total cost of pizza
-}
pizzaPricing :: Float -> Int -> Int -> Float -> Float
pizzaPricing r t s d = 
    pick ((pi * r * r * 0.002 + fromIntegral t * 0.001 * pi * r * r + fromIntegral s * 0.55) * 1.5 + d * 0.9)

main = do
    print $ pizzaPricing 30 1 20 30
    
