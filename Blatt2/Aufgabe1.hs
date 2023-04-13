import Data.Char (isLetter, toLower)

alphabet :: String
alphabet = ['a' .. 'z']

countChar :: String -> Char -> Int
countChar str c =
  countChar' str c 0
  where
    countChar' [] _ n = n
    countChar' (x : xs) c n =
      countChar' xs c $
        if x == c
          then n + 1
          else n

charCounts :: String -> [Int]
charCounts str =
  map (countChar str) alphabet

sanitize :: String -> String
sanitize str = filter isLetter $ map toLower str

isPangram :: String -> Bool
isPangram str =
  foldr (\count acc -> (count >= 1) && acc) True $ charCounts (sanitize str)

main :: IO ()
main = do
  line <- getLine
  if isPangram line
    then putStrLn "The sentence is a pangram!"
    else putStrLn "The sentence isn't a pangram!"
