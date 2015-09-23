module TypeClassThing where

class FooBar a where
    foo :: a -> String

instance FooBar Int where
    foo _ = "foobar"

data LogEntry = Error String
    | Info String
    deriving(Show)
