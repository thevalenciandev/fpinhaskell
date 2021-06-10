speller :: [[Char]] -> [Char]
speller []       = []
speller [x]      = spell x
speller [x,y]    = spell x ++ ", and " ++ spell y
speller (x:xs)   = spell x ++ ", " ++ speller xs

spell :: String -> String
spell all@(x:_) = x : " is for " ++ all
