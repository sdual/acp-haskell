acpGcd :: Int -> Int -> Int
acpGcd a b
  | rmdr == 0 = b
  | otherwise = acpGcd b rmdr
    where rmdr = a `mod` b
          q = a `div` b
