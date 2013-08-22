{-# LANGUAGE CPP #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- With GHC 7.7, we get the following error:
--     Could not deduce (BoundsOf (a -> a) ~ Integer)

-- 1) Where does this Integer come from?
--
-- 2) Change  BoundsOf (a->a)    = Int
--      into  BoundsOf (a->a)    = Integer
--
--    You get the same error message, although clearly BoundsOf (a -> a) ~ Integer
--    (you just wrote that down).
--    Even if the order matters in closed type families,
--    is this not the wrong error message?
--
-- 3) If you flip the order so that the (a->a) case comes first,
--    it works (with both it Int or Integer). I don't quite understand why.

module Test where


data Vector a = Vector a
data Matrix a = Matrix a

class Build f where
    build' :: BoundsOf f -> f -> ContainerOf f



#if __GLASGOW_HASKELL__ <= 706
-- Normal type families with GHC 7.6, works well.

type family BoundsOf x
type instance BoundsOf (a->a)    = Int
type instance BoundsOf (a->a->a) = (Int,Int)

type family ContainerOf x
type instance ContainerOf (a->a) = Vector a
type instance ContainerOf (a->a->a) = Matrix a

#else
-- Closed type families for GHC 7.8

type family BoundsOf x where
    BoundsOf (a->a->a) = (Int,Int)
    BoundsOf (a->a)    = Int

type family ContainerOf x where
    ContainerOf (a->a)    = Vector a
    ContainerOf (a->a->a) = Matrix a

#endif


instance (Num a) => Build (a->a) where
    build' = buildV


buildV :: (Integral a, Num b) => a -> (b -> c) -> Vector c
buildV _ _ = undefined
