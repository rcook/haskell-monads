
-- used only for standard operations on monads

import Control.Monad

-- a type constructor that allows representing two kinds of errors 

data ValueOrDetails v =
    Value v |
    TooLargeError String |
    TooSmallError String

-- use it to define a monad

instance Monad ValueOrDetails where
    (TooLargeError s) >>= f     = (TooLargeError s)
    (TooSmallError s) >>= f     = (TooSmallError s)
    (Value e) >>= f             = f e
    return                      = Value

tooSmall :: v -> ValueOrDetails v
tooSmall v = (TooSmallError (show v))

tooLarge :: v -> ValueOrDetails v
tooLarge v = (TooLargeError (show v))

-- need a way to print the monad

instance Show a => Show (ValueOrDetails a) where
  show (Value v) = show v
  show (TooLargeError s) = "Value too large: " ++ s
  show (TooSmallError s) = "Value too small: " ++ s

-- use it to get the max value of an integer list where
-- values are required to be in the interval [l,u]
-- if the list is empty we return l

-- start with a recursive version based on do

maxInRange        :: Integer -> Integer -> [Integer] -> ValueOrDetails Integer
maxInRange l u values     = maxInRangeRec l u (return l) values

maxInRangeRec     :: Integer -> Integer -> ValueOrDetails Integer -> 
                             [Integer] -> ValueOrDetails Integer
maxInRangeRec l u vod []     = vod
maxInRangeRec l u vod (x:xs) = 
             do           
               prev <- vod
               nvod <- if x > u then (tooLarge x) 
                       else if x < l then (tooSmall x)
                       else return (max x prev)
               rest <- maxInRangeRec l u (return nvod) xs
               return rest

-- alternate version based on foldM

f :: Integer -> Integer -> Integer -> Integer -> ValueOrDetails Integer
f l u acc val = 
  if val > u then (tooLarge val) 
             else if val < l then (tooSmall val)
             else return (max val acc)

maxInRangeF :: Integer -> Integer -> [Integer] -> ValueOrDetails Integer
maxInRangeF l u values = foldM (f l u) l values

l1 = [11,12,10,17,13]
l2 = [11,5,10,17,2,13]
l3 = [11,25,10,17,80,13]

main = do
         print "recursive version using do ..."
         print (maxInRange 10 20 l1)
         print (maxInRange 10 20 l2)
         print (maxInRange 10 20 l3)
         print "foldM version ..."
         print (maxInRangeF 10 20 l1)
         print (maxInRangeF 10 20 l2)
         print (maxInRangeF 10 20 l3)
