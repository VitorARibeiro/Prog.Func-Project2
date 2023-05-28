import Data.List (delete)

removeDuplicatesOnce :: Eq a => [a] -> [a]
removeDuplicatesOnce [] = []
removeDuplicatesOnce (x:xs)
  | x `elem` xs = x : removeDuplicatesOnce (delete x xs)
  | otherwise = x : removeDuplicatesOnce xs

main :: IO ()
main = do
  let myList = [1, 1, 1, 2, 2, 3]
  let uniqueList = removeDuplicatesOnce myList
  print uniqueList