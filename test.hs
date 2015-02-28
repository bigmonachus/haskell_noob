--------------------------------------------------------------------------------
-- Chapter 2
--------------------------------------------------------------------------------

-- My first function
myDrop n xs = if n <= 0 || null xs
                  then xs
                  else myDrop (n - 1) (tail xs)

-- Partial application
drop3 = drop 3

--------------------------------------------------------------------------------
-- Chapter 3
--------------------------------------------------------------------------------

data BookInfo = Book Int String [String]
              deriving (Show)
data MagazineInfo = Magazine Int String [String]
              deriving (Show)

myInfo = Book 13241234123 "BookTitle" [ "Lastname", "Another", "Yuppington" ]

type CustomerID = Int
type ReviewBody = String

data Review = Review BookInfo CustomerID ReviewBody

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  | Empty
                  deriving(Show)

-- Generate accesors.
data Person = Person {
            personID    :: Int,
            personName  :: String
            } deriving (Show)

data Vec2 = Vec2 {
                 x :: !Float,
                 y :: !Float
                 } deriving(Show)

mySecond (_:x:_)    = Just x
mySecond _          = Nothing

pluralize :: String -> [Int] -> [String]
pluralize thing counts = map plural counts where
    plural 0 = "no " ++ thing ++ "s"
    plural 1 = "one " ++ thing
    plural n = show n ++ " " ++ thing ++ "s"

data Fruit = Apple | Orange
           deriving(Show)

-- pattern matchin
whichFruit :: String -> Fruit
whichFruit f = case f of
                   "apple" -> Apple
                   "orange" -> Orange

-- guards
nodesAreSame (Node a _ _) (Node b _ _) | a == b = Just a
nodesAreSame _ _ = Nothing

treeHeight Empty        = 0
treeHeight (Node _ b c) = 1 + treeHeight b + treeHeight c


