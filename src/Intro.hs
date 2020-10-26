test :: Int
test = 1 + 1

addOne :: Int -> Int
addOne x = x + 1

times :: Int -> Int -> Int
times x y = x * y

timesThree :: Int -> Int
timesThree = times 3

applyTwo :: (Int -> Int) -> (Int -> Int) -> Int
applyTwo fn gn = gn (fn 2)
