-- MyFunctor

data MyData a = MyData a | EmptyData deriving (Show)

instance Functor MyData where
    fmap _ EmptyData = EmptyData
    fmap f (MyData a) = (MyData (f a))
    
    