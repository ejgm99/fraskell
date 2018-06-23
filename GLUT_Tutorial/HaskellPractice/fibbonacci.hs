fibonacci :: Integer -> [Integer]
fibonacci 0 = [1]
fibonacci 1 = [1,1]
fibonacci x = (fibonacci (x-1)) ++ [last (fibonacci (x-1)) + last (init (fibonacci (x-1)))]
