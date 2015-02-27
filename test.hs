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

myInfo = Book 13241234123 "BookTitle" [ "Lastname", "Another", "Yuppington" ]
