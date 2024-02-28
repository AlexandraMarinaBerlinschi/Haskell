data Pereche a b = MyP a b deriving Show
newtype Lista a = MyL [a] deriving Show 

class MyOp m where 
    myZip :: m a -> m b -> m (Pereche a b)

--REZOLVARE : Clasa MyOp pentru tipul de date Lista: 
instance MyOp Lista where
    myZip (MyL xs) (MyL ys) = MyL (zipWith MyP xs ys)