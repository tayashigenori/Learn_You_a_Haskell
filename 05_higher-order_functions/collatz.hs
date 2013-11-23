chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n = n: chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

listNumLongChains :: Integer -> Int -> [Integer]
listNumLongChains n min = filter isLongChain [1..n]
    where isLongChain n = length(chain n) > min

listLongChains :: Integer -> Int -> [[Integer]]
listLongChains n min = filter isLong (map chain [1..n])
    where isLong xs = length xs > min