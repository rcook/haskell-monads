
{-
This is intended to be a simple, uncluttered example of using the State monad.
The state is kept fairly simple, there are a few operations of different
signatures, there are essentially no other monads, and especially no monad
transformers.

The application is a unique symbol generator with a twist: there are two
counters, one advanced automatically with each symbol, and the other advanced
manually (reinitializing the first one.)

One compromise I've made is bothersome but I think necessary: by ignoring
arithmetic overflow I've essentially banished any notion of error handling.
That has the advantage of avoiding the introduction of another monad (surely
the right way to handle errors in Haskell), but diminishes the realism I was
hoping to achieve.
-}


import Control.Monad.State

-- 'SymbolCounter' is the actual state the State monad will manage.

-- Use a pair of counter, ('hi', 'lo'), to generate symbols of the form
-- sym_'hi'_'lo' starting with sym_0_0. When a symbol is generated,
-- the 'lo' counter is advanced, so the next one is sym_0_1. But there is an
-- operation, 'advanceHi', that advances the other 'hi' and resets the 'lo'
-- one, so after calling it the next symbol would be sym_1_0.
type SymbolCounter = (Int,Int)

-- |Create an initialized state
initialCounters :: SymbolCounter
initialCounters = (0,0)
  
-- |Advance the low counter
advanceLo :: SymbolCounter -> SymbolCounter
advanceLo (hi,lo) = (hi,lo+1)

-- |Advance the high counter while zeroing the low one
advanceHi :: SymbolCounter -> SymbolCounter
advanceHi (hi,_) = (hi+1,0)
                 
-- |Create a symbol and advance the low counter
makeSymbol :: SymbolCounter -> String
makeSymbol (hi,lo) = "sym_" ++ (show hi) ++ "_" ++ (show lo)

{-
These are the State monad operations.
-}

-- |Initialize the state.
-- Notice the () since nothing is returned.
--initsym :: State SymbolCounter ()
--initsym = state $ \c -> ((), initialCounters)

-- |Generate a symbol and advance the state normally.
-- Notice that a String is returned.
getsym :: State SymbolCounter String
getsym = state $ \c -> (makeSymbol c, advanceLo c)
-- could this reasonably have been written in terms of 'modify'?

-- !Start a new sequence of symbols by advancing the high counter.
-- No symbol is returned hence the (). 
nextseq :: State SymbolCounter ()
nextseq = state $ \c -> ((), advanceHi c)
-- is 'modify advanceHi' better?

-- !Get a bunch of symbols and return the last one.
-- This is not entirely satisfying as perhaps you'd like to be able to work
-- with several states, or kinds of state, at the same time. How?
gymnastics :: State SymbolCounter String 
gymnastics = do
  _ <- getsym
  _ <- getsym
  nextseq -- new sequence of symbols ('hi' incremented, 'lo' zeroed)
  _ <- getsym
  d <- getsym
  return d

-- Is this the right way to do set it up?
main :: IO ()
main = do
  print (evalState gymnastics initialCounters)
  
