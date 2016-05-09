
symbolStep :: (Int,Int) -> (String,(Int,Int))
symbolStep (c1,c2) = let sym = "sym_" ++ (show c1) ++ "_" ++ (show c2)
                      in (sym, (c1,c2+1))



main = let preState = (2,3) 
           (sym,postState) = symbolStep preState
        in 
           do
             print sym
             print postState