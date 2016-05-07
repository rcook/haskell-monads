
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

-- use it to get the max value of an integer list where
-- values are required to be in the interval [l,u]
-- if the list is empty we return l

maxInRange        :: Integer -> Integer -> [Integer] -> ValueOrDetails Integer
maxInRange l u values     = maxInRangeRec l u (return l) values

maxInRangeRec     :: Integer -> Integer -> ValueOrDetails Integer -> 
		     	     [Integer] -> ValueOrDetails Integer
maxInRangeRec l u vod []     = vod
maxInRangeRec l u vod (x:xs) = 
             do           
	       prev <- vod
               nvod <- if x > u then (TooLargeError (show x)) 
                       else if x < l then (TooSmallError (show x))
                       else return (max x prev)
               rest <- maxInRangeRec l u (return nvod) xs
               return rest

-- need a way to print the result

instance Show a => Show (ValueOrDetails a) where
  show (Value v) = show v
  show (TooLargeError s) = "Value too large: " ++ s
  show (TooSmallError s) = "Value too small: " ++ s

l1 = [11,12,10,17,13]
l2 = [11,5,10,17,2,13]
l3 = [11,25,10,17,80,13]

main = do
         print (maxInRange 10 20 l1)
	 print (maxInRange 10 20 l2)
	 print (maxInRange 10 20 l3)
