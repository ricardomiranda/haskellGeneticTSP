module City where

data City = City { id :: Int
                 , pos :: (Float, Float) }  deriving Show

newCity :: Int -> (Float, Float) -> City
newCity i (x, y) = City { City.id = i
                        , pos = (x, y) }

-- to save expensive computations distances will be left squared
squareDistance :: City -> City -> Float
squareDistance a b =
  (fst (pos a) - fst (pos b)) ^ 2 + (snd (pos a) - snd (pos b)) ^ 2
