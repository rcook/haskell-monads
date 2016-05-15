
# Using Monads as an Engineering Tool in Haskell

This is an exercise in understanding how and to what degree Haskell 
monads provide a valuable software valuable engineering tool. It's my 
personal exploration while I'm learning the language, so I would warn 
readers against treating it as definitive. I'll often be checking in code 
just to get feedback from others, so it will sometimes be either wrong or at
least bad or controversial. I might also keep the bad examples around if they 
turn out to be interesting. 

## Defining and Using New Monads

| Example | Description |
| ------- | ----------- |
| [ValueOrDetails.hs] | Represent correct values or two error states -- kind of like Control.Monad.Either but with an extra error state. |

## Making Use of Tools in Control.Monad

| Example | Description |
| ------- | ----------- |
| [StatefulSymbolGen.hs] | Use the State monad for generating unique symbols. |
