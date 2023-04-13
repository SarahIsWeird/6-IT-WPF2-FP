data Signal
  = Red
  | Yellow
  | Green

-- deriving (Eq, Show)

instance Eq Signal where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show Signal where
  show Red = "Red"
  show Yellow = "Yellow"
  show Green = "Green"

stopwhen :: Signal -> Bool
stopwhen Red = True
stopwhen _ = False

stopwhen2 :: Signal -> Bool
stopwhen2 c
  | c == Red = True
  | otherwise = False
