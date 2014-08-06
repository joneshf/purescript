module Main where

data Foo = Foo
         | Bar

deriving instance eqFoo :: Eq Foo

main = Debug.Trace.print $ Foo == Foo
