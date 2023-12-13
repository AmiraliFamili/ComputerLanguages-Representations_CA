import Data.List

main :: IO ()
main = print (filter tester1 generator1)

-- Generator 1 Test Function
------------------------------------------------------------

x_generator1 :: Int 
x_generator1 =
    length [t | t <- ts, t `elem` g] 
    where
        g = generator1
        ts = 
            [ ( 2,15,14,11)
            , ( 4,31,27, 9)
            , ( 6,47,10, 8)
            , ( 9, 3,23, 6) , (11,19, 6, 5) , (13,35,19, 3) , (15,51, 2, 2) , (18, 6,16,12)
            , (20 ,22 ,29 ,10) , (22,38,11, 9) ]
            

-- Tester 1 Test Function
------------------------------------------------------------

x_tester1 :: Int 
x_tester1 =
    length [t | t <- ts, tester1 t] 
    where
        ts =
            [ ( 6,59,17,24), ( 6,59,17,34)
            , ( 6,59,27,14) , ( 6,59,27,41) , ( 8,59,12,46) , (16,59, 7,24) , (16,59, 7,42)
            , (16,59, 7,43) , (16 ,59 ,27 ,40) , (18,59, 2,46) ]


-- Code For Light Show 
------------------------------------------------------------

generator1 :: [(Int, Int, Int, Int)]
generator1 = [(hr, mn, dy, mt) | hr <- [1..23], mn <- [1..59], dy <- [1..30], mt <- [1..12]]

tester1 :: (Int, Int, Int, Int) -> Bool 
tester1 (hr, mn, dy, mt) 
    | mn /= 59 = magic(hr, mn, dy, mt) && magic(hr, mn, (dy+1), mt) && litSegments (hr, (mn+1), (dy+1), mt) == (litSegments(hr, mn, dy, mt) + litSegments(hr, mn, (dy+1), mt)) `div` 2
    | mn == 59 =  magic(hr, mn, dy, mt) && magic(hr, mn, (dy+1), mt) && litSegments ((hr+1), 0, (dy+1), mt) == (litSegments(hr, mn, dy, mt) + litSegments(hr, mn, (dy+1), mt)) `div` 2
    | otherwise = magic(hr, mn, dy, mt) && magic(hr, mn, (dy+1), mt) && litSegments (1, 0, (dy+1), mt) == (litSegments(hr, mn, dy, mt) + litSegments(hr, mn, (dy+1), mt)) `div` 2

magic :: (Int, Int, Int, Int) -> Bool 
magic (hr, mn, dy, mt) = different (hr, mn, dy, mt) && prime (litSegment hr + litSegment mn + litSegment dy + litSegment mt) 

different :: (Int, Int, Int, Int) -> Bool 
different (hr, mn, dy, mt) = length ("123456789" \\ (show hr ++ show mn ++ show dy ++ show mt)) <= 2

litSegments :: (Int, Int, Int, Int) -> Int 
litSegments (hr, mn, dy, mt) = (litSegment hr + litSegment mn + litSegment dy + litSegment mt) 

getSegment :: Int -> Int 
getSegment x
    | x == 0 = 6 
    | x == 1 = 2 
    | x == 2 = 5  
    | x == 3 = 5 
    | x == 7 = 3 
    | x == 8 = 7 
    | x == 9 = 6 
    | otherwise = x 

litSegment :: Int -> Int 
litSegment x 
    | x >= 10 = getSegment(x `mod` 10) + getSegment(x `div` 10) 
    | otherwise = getSegment x + getSegment 0

prime :: Int -> Bool
prime = not . factoriasable 2

factoriasable :: Int -> Int -> Bool
factoriasable f n 
    | f * f <= n = n `mod` f == 0 || factoriasable (f+1) n
    | otherwise = False