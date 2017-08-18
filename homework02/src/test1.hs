data Maybe2 a = Nothing2 | Just2 a
  deriving (Show, Eq)

instance Ord a => Ord (Maybe2 a) where
  compare Nothing2 Nothing2         = EQ
  compare (Just2 _) Nothing2        = GT
  compare Nothing2 (Just2 _)        = LT
  compare (Just2 val1) (Just2 val2) = compare val1 val2

-------------------------------------------------
-- data Person = Persona String Int
--   deriving Show

-- instance Show Person where
--	show (Persona nombre edad) = "Nombre: " ++ nombre
--				  ++ " Edad: " ++ show edad

-------------------------------------------------
data IntList = Empty | Const Int IntList

mySum :: IntList -> Int
mySum Empty             = 0
mySum (Const i intList) = i + mySum intList

-- mySum $ Const 4 $ Const 9 Empty
-- 13

-------------------------------------------------
data IntListInf = Const2 Int IntListInf
  deriving Show

myRepeat :: Int -> IntListInf
myRepeat n = Const2 n (myRepeat n)

myTake :: Int -> IntListInf -> [Int]
myTake 0 _                     = []
myTake n (Const2 i intListInf) = i : (myTake (n-1) intListInf)
