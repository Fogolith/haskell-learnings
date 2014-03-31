data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

--       v type constructor
data BookReview = BookReview BookInfo CustomerID String
--                     ^ value constructor (often match)

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

-- type synonym just creates and easyer way to refer
-- to regular types
type BookRecord = (BookInfo, BookReview)
type CustomerID = Int
type ReviewBody = String

type CardHolder = String
type CardNumber = String
type Address = [String]

-- algebratic data types can have more than one value contuctor
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- pattern matching against algebraic data types
bookID (Book id title authors) = id

-- using wildcard
nicerID (Book id _ _) = id

-- using record syntax
data Customer = Customer {
    customerID       :: CustomerID
    ,customerName    :: String
    ,customerAddress :: Address
} deriving (Show)

-- can then declare like:
customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]
-- OR
customer2 = Customer {
              cusomterID = 271828
            , customerAddress = ["10485 Disk Drive",
                                  "ABC Land, OH 44136",
                                  "USA"]
            , customerName = "Jane Q. Citizen"
}

-- parameterised types -- using type variables in type declarations
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
-- 'a' can by any type here, as long as its the same

-- Use Maybe to wrap values to handle errors better

-- 'local' variables (and functions)
lend amount balance = let reserve = 100
                          newBalance =  balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance

lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
      where reserve = 100
            newBalance = balance - amount

pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"

-- case expression
fromMaybe defval wrapped =
  case wrapped of
    Nothing ->    defval
    Just value -> value

-- Guards:
lend3 amount balance
      | amount <= 0             = Nothing
      | amount > reserve * 0.5 = Nothing
      | otherwise              = Just newBalance
    where reserve = 100
          newBalance = balance - amount





