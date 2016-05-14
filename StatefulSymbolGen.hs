
import Control.Monad.State

type SymbolCounter = (Int,Int)

-- how do you initialize the state?
initial :: SymbolCounter
initial = (0,0)
  
-- how do you advance the state?
advance :: SymbolCounter -> SymbolCounter
advance (c1,c2) = (c1,c2+1)

next :: SymbolCounter -> SymbolCounter
next (c1,c2) = (c1+1,0)
                 
-- how do you create a symbol?
makeSymbol :: SymbolCounter -> String
makeSymbol (c1,c2) = "sym_" ++ (show c1) ++ "_" ++ (show c2)

-- operations using the State monad

initsym :: State SymbolCounter ()
initsym = state $ \p -> ((), initial)

stateValue :: State SymbolCounter SymbolCounter
stateValue = modify advance >> get

ms :: State SymbolCounter String
ms = state $ \p -> (makeSymbol p, advance p)

roll :: State SymbolCounter ()
roll = modify next
  
stuff :: State SymbolCounter String 
stuff = do
  a <- ms
  b <- ms
  roll
  c <- ms
  d <- ms
  return d

main = do
  print (evalState stuff initial)
  
