{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens

data Coord = Coord {_x :: Int, _y :: Int}
  deriving Show

$(makeLenses ''Coord)

--_1 :: Lens (a, x) (b, x) a b
--_1 f = \(a, x) -> (,x) <$> f b

--ghci> :t z
--z :: Functor f => (a -> f b) -> s -> f t
--ghci> :t x
--x :: Functor f => (Int -> f Int) -> Coord -> f Coord

-- lenses generate by GHC
--
-- 
-- ghci> :set -XTemplateHaskell
-- ghci> import Language.Haskell.TH
-- ghci> putStrLn $(stringE . pprint =<< makeLenses ''Coord)
-- x :: Control.Lens.Type.Lens' Main.Coord GHC.Types.Int
-- x f_0 (Main.Coord x1_1
--                  x2_2) = GHC.Base.fmap (\y1_3 -> Main.Coord y1_3 x2_2) (f_0 x1_1)
-- {-# INLINE x #-}
-- y :: Control.Lens.Type.Lens' Main.Coord GHC.Types.Int
-- y f_4 (Main.Coord x1_5
--                   x2_6) = GHC.Base.fmap (\y1_7 -> Main.Coord x1_5 y1_7) (f_4 x2_6)
-- {-# INLINE y #-}


x6 :: Lens' Coord Int
x6 f (Coord x y) = fmap (\x' -> Coord x' y) (f x)
{-# INLINE x6 #-}

-- function that take setter and getter and creates lens from it
-- definition base on presentation by Twan van Laarhoven
-- https://twanvl.nl/files/lenses-talk-2011-05-17.pdf
-- — (get,set) → FTLens
-- fromGetSet :: Functor f ⇒ (α → β) → (β → α → α) → (β → f β) → α → f α
-- fromGetSet get set return a =
--   fmap (flip set a) (return (get a))
-- minor different it is modified to match lens defition from library
mklens' :: Functor f => (a -> b) -> (a -> b -> a) -> (b -> f b) -> a -> f a
mklens' get set return a = fmap (set a) (return (get a))

y1 :: Lens Coord Coord Int Int
y1 = mklens' _y (\r g -> r{_y=g})

incrementX = over x (+1)
incrementY = over y (+1)

-- Functor f => (Int -> f Int) -> Coord -> f Coord
--x :: Lens Coord Coord Int Int
--x = fromGetSet _x (\r g -> r{_x=g})
x1 :: Lens' Coord Int
x1 = lens _x (\r g -> r{_x=g})

-- definition in style of https://rafal.io/posts/haskell-lenses-notes.html
-- direct definition of lense without use of helper functions
x2 :: Lens' Coord Int
x2 f r = fmap (\g -> r {_x = g}) (f $ _x r)

-- manual definition in style of https://hackernoon.com/taking-a-closer-look-at-lenses-c0304851d54c
x3 :: Lens' Coord Int
x3 f r@Coord{_x = x} = f x <&> \g -> r{_x = g}

-- manual definition in style of https://hackernoon.com/taking-a-closer-look-at-lenses-c0304851d54c
-- but without pattern matching
x4 :: Lens' Coord Int
x4 f r = (f $ _x r) <&> \g -> r{_x = g}

-- definition in style of https://rafal.io/posts/haskell-lenses-notes.html
-- direct definition of lense without use of helper functions
-- user operator <$> instead of fmap
x5 :: Lens' Coord Int
x5 f r = (\g -> r {_x = g}) <$> (f $ _x r)


main :: IO ()
main = putStrLn "Hello, Haskell!"

