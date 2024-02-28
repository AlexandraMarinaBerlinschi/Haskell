newtype BM a = BM { getBM :: (String, Maybe a)} deriving Show

instance Functor BM where 
    fmap f ma = f <$> ma

instance Applicative BM where 
    pure = return 
    mf <*> ma = do
        f <- mf
        f <$> ma 
    
testBM :: BM Int 
testBM = ma >>= f
    where 
        ma = BM ("ana are mere", Just 7)
        f x = BM ("si pere!", if even x then Just x else Nothing)

--Bm {getBM = ("ana are mere si pere!",Noting)}

--REZOLVARE : instanta clasei Monad pentru tipul BM : 
instance Monad BM where
    return x = BM ("", Just x)
    ma >>= f = case getBM ma of
        (s, Nothing) -> BM (s, Nothing)
        (s, Just x) -> let (s', mb) = getBM (f x)
                       in BM (s ++ s', mb)
