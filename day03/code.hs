import System.IO
import System.Environment   
import Data.List


parser :: String -> [Int]
parser [] =  []
parser (str:strs) | str == '0' = 0: parser strs
                  | otherwise  = 1: parser strs


counter :: [[Int]] -> [Int]
counter [x] = x
counter (x:xs) = zipWith(+) x (counter xs)


rev :: [Int] -> [Int]
rev lst = revhelper lst []

revhelper :: [Int] -> [Int] -> [Int]
revhelper [] accum = accum
revhelper (x:xs) accum = revhelper xs (x:accum)


toDec :: [Int] -> Int
toDec lst = (toDecHelper (rev lst))

toDecHelper :: [Int] -> Int
toDecHelper [x] = x
toDecHelper (x:xs) = (x + 2 * toDecHelper(xs))


index :: Int -> [Int] -> Int
index 0 lst    = head lst
index n (x:xs) = index (n - 1) xs



oxygen :: Int -> [[Int]] -> [Int]
oxygen n [x] = x
oxygen n lst = oxygen (n + 1) (filter (\x -> (index n x == val)) lst)
               where m     = (fromIntegral (length lst))/2
                     count = index n (counter lst)
                     val   = fromEnum ((fromIntegral count) >= m)


cotwo :: Int -> [[Int]] -> [Int]
cotwo n [x] = x
cotwo n lst = cotwo (n + 1) (filter (\x -> (index n x == val)) lst)
              where m     = (fromIntegral (length lst))/2
                    count = index n (counter lst)
                    val   = fromEnum ((fromIntegral count) < m)


main :: IO()
main = do  
  args     <- getArgs
  handle   <- openFile (head args) ReadMode
  contents <- hGetContents handle

  let parse = map parser (words contents)
      n     = length parse
      m     = (fromIntegral n)/2

  let counts = counter parse
      bin1  = map (\x -> (fromEnum (fromIntegral x >= m))) counts
      bin2  = map (\x -> (fromEnum (fromIntegral x <  m))) counts
      val1  = toDec bin1
      val2  = toDec bin2

  print (val1 * val2) -- Part 1

  let oxval = toDec (oxygen 0 parse)
      coval = toDec (cotwo  0 parse)

  print (oxval * coval) -- Part 2
