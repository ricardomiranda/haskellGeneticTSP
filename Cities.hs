module Cities where

import City

type Cities = [ City ] 

findCity :: Cities -> Int -> City
findCity cs id = head $ filter (\ c -> City.id c == id) cs
