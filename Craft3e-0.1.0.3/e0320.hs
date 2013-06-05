averageThree :: Int -> Int -> Int -> Float
averageThree a b c = (fromIntegral (a+b+c))/3

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c = length $ filter (> avg) the_three 
	where avg = averageThree a b c
	      the_three = [fromIntegral a, fromIntegral b, fromIntegral c]
