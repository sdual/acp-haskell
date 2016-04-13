-- Euclidean Algorithm
acpGcd :: Int -> Int -> Int
acpGcd a b
  | r == 0 = b
  | otherwise = acpGcd b r
    where r = a `mod` b
          q = a `div` b

-- Extended Euclidean Algorithm (find Bézout's identity)
extendedGcd :: Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int)
extendedGcd a b x1 x2 y1 y2
  | r == 0 = (r, x2, y2)
  | otherwise = extendedGcd b r x2 (x1 - q * x2) y2 (y1 - q * y2)
    where r = a `mod` b
          q = a `div` b
