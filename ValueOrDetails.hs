
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
-- values are requried to be between two given values

maxInRange        :: Integer -> Integer -> [Integer] -> ValueOrDetails Integer
maxInRange l u []     = (Value l)
maxInRange l u (x:xs) = 
           let rest = maxInRange l u xs in
             case rest of Value v -> 
                             if x > u then (TooLargeError (show x)) 
                             else if x < l then (TooSmallError (show x))
                             else (Value (max x v))
                          TooLargeError s -> TooLargeError s
                          TooSmallError s -> TooSmallError s

-- need a way to print the result

instance Show a => Show (ValueOrDetails a) where
  show (Value v) = show v
  show (TooLargeError s) = "Value too large: " ++ s
  show (TooSmallError s) = "Value too small: " ++ s

l1 = [11,12,10,17,13]
l2 = [11,5,10,17,13]
l3 = [11,25,10,17,13]

main = do
         print (maxInRange 10 20 l1)
	 print (maxInRange 10 20 l2)
	 print (maxInRange 10 20 l3)
