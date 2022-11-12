-- init value
pizza :: Float
topping :: Float
sauce :: Float
multi :: Float
delivery :: Float

pizza = 0.002
topping = 0.001
sauce = 0.55
multi = 1.5
delivery = 0.9

-- get the area of circle
areaCircle :: Float -> Float
areaCircle r = pi * r * r

-- get cost of pizze base
pizzaBaseCost :: Float -> Float
pizzaBaseCost r = areaCircle r * pizza

-- get cost of topping
toppingCost :: Int -> Float -> Float
toppingCost n r = (fromIntegral n) * (areaCircle r) * topping

-- get cost of sauce
sauceCost :: Int -> Float
sauceCost n = fromIntegral n * sauce

-- mutli 1.5 for profit
multiCost :: Float -> Float
multiCost price = multi * price

-- get cost of delivery
deliveryCost :: Float -> Float
deliveryCost mile = delivery * mile

-- x : number you want rounded, n : number of decimal places you want...
truncate' :: Float -> Int -> Float
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

--get pizza Price
pizzaPricing :: Float -> Int -> Int -> Float -> Float
pizzaPricing diameter numberTopping numberSauce distance = 
    -- calculate the cost
    truncate' (multiCost (pizzaBaseCost diameter + (toppingCost numberTopping diameter) + sauceCost numberSauce) + deliveryCost distance) 2

main = do
    -- result is 111.47
    print $ pizzaPricing 40 10 20 5
    
