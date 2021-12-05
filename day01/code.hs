import System.IO
import System.Environment   
import Data.List  


counter :: [Integer] -> Integer -> Integer -> Integer
counter [] prev count = count
counter (x:xs) prev count | (x > prev) = counter xs x (count + 1)
                          | otherwise  = counter xs x count


-- Custom implimentation of "take" for practice.
tk :: Int -> [Integer] -> [Integer]
tk n []     = []
tk 0 l      = []
tk n (x:xs) = x : tk (n - 1) xs


n_avg :: Int -> [Integer] -> [Integer]
n_avg n lst | (length lst) < n = []
            | otherwise        = sum nlst : n_avg n rest
            where nlst = tk n lst
                  rest = tail lst


f :: [String] -> [Integer]
f = map read


main :: IO()
main = do  

  args     <- getArgs
  handle   <- openFile (head args) ReadMode
  contents <- hGetContents handle

  let parse = f lst
      lst   = words contents

  let part1  = counter xs x 0
      (x:xs) = parse

  print part1

  let part2  = counter xs x 0
      (x:xs) = n_avg 3 parse

  print part2
