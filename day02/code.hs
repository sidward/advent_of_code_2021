import System.IO
import System.Environment   
import Data.List  


f :: [String] -> [Integer]
f = map read

evenelms []  = []
evenelms [x] = []
evenelms (x:xs) = head xs : evenelms (tail xs)

oddelms []  = []
oddelms [x] = [x]
oddelms (x:xs) = x : evenelms xs


move :: (Integer, Integer) -> String -> Integer -> (Integer, Integer)
move (x, y) direction value | direction == "forward" = (x + value, y)
                            | direction == "down"    = (x, y + value)
                            | direction == "up"      = (x, y - value)

move2 :: (Integer, Integer, Integer) -> String -> Integer -> (Integer, Integer, Integer)
move2 (x, y, a) direction value | direction == "forward" = (x + value, y + a * value, a        )
                                | direction == "down"    = (x,         y,             a + value)
                                | direction == "up"      = (x,         y,             a - value)


composite :: (Integer, Integer) -> [String] -> [Integer] -> (Integer, Integer)
composite (x, y) [] _ = (x, y)
composite (x, y) _ [] = (x, y)
composite (x, y) (dir:dirs) (val:vals) = composite (newx, newy) dirs vals
  where (newx, newy) = move (x, y) dir val


composite2 :: (Integer, Integer, Integer) -> [String] -> [Integer] -> (Integer, Integer, Integer)
composite2 (x, y, a) [] _ = (x, y, a)
composite2 (x, y, a) _ [] = (x, y, a)
composite2 (x, y, a) (dir:dirs) (val:vals) = composite2 (newx, newy, newa) dirs vals
  where (newx, newy, newa) = move2 (x, y, a) dir val
  

main :: IO()
main = do  

  args     <- getArgs
  handle   <- openFile (head args) ReadMode
  contents <- hGetContents handle

  let parse = words contents
      nums  = f (evenelms parse)
      drct  = (oddelms parse)

  let part1 = composite (0, 0) drct nums
      (hor, ver) = part1

  print (hor * ver)

  let part2 = composite2 (0, 0, 0) drct nums
      (hor, ver, _) = part2

  print (hor * ver)
