{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Test where

import Foreign.Storable


data Vector a = Vector a
data Matrix a = Matrix a


type family RealOf x

type instance RealOf Double = Double
type instance RealOf Float = Float
type family ComplexOf x

type family SingleOf x

type instance SingleOf Double = Float
type instance SingleOf Float  = Float

type family DoubleOf x

type instance DoubleOf Double = Double
type instance DoubleOf Float  = Double

type family ElementOf c

type instance ElementOf (Vector a) = a
type instance ElementOf (Matrix a) = a


class (Storable a) => Element a where
    subMatrixD :: (Int,Int) -- ^ (r0,c0) starting position 
               -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
               -> Matrix a -> Matrix a
    subMatrixD = undefined -- subMatrix'
    transdata :: Int -> Vector a -> Int -> Vector a
    transdata = undefined -- transdataP -- transdata'
    constantD  :: a -> Int -> Vector a
    constantD = undefined -- constantP -- constant'


instance Element Float where
    transdata  = undefined
    constantD  = undefined

instance Element Double where
    transdata  = undefined
    constantD  = undefined


fromList :: Storable a => [a] -> Vector a
fromList = undefined

fromLists :: Element t => [[t]] -> Matrix t
fromLists = undefined


-- -- | Matrix product and related functions
-- class Element e => Product e where
--     -- | matrix product
--     multiply :: Matrix e -> Matrix e -> Matrix e
--     -- | dot (inner) product
--     dot        :: Vector e -> Vector e -> e
--     -- | sum of absolute value of elements (differs in complex case from @norm1@)
--     absSum     :: Vector e -> RealOf e
--     -- | sum of absolute value of elements
--     norm1      :: Vector e -> RealOf e
--     -- | euclidean norm
--     norm2      :: Vector e -> RealOf e
--     -- | element of maximum magnitude
--     normInf    :: Vector e -> RealOf e

-- instance Product Float where
--     norm2      = toScalarF Norm2
--     absSum     = toScalarF AbsSum
--     dot        = dotF
--     norm1      = toScalarF AbsSum
--     normInf    = maxElement . vectorMapF Abs
--     multiply = multiplyF

-- instance Product Double where
--     norm2      = toScalarR Norm2
--     absSum     = toScalarR AbsSum
--     dot        = dotR
--     norm1      = toScalarR AbsSum
--     normInf    = maxElement . vectorMapR Abs
--     multiply = multiplyR



class Build f where
    build' :: BoundsOf f -> f -> ContainerOf f

#if __GLASGOW_HASKELL__ <= 706
type family BoundsOf x

type instance BoundsOf (a->a->a) = (Int,Int)
type instance BoundsOf (a->a) = Int
#else
type family BoundsOf x where
    BoundsOf (a->a->a) = (Integer,Integer)
    BoundsOf (a->a) = Integer
#endif

#if __GLASGOW_HASKELL__ <= 706
type family ContainerOf x

type instance ContainerOf (a->a->a) = Matrix a
type instance ContainerOf (a->a) = Vector a
#else
type family ContainerOf x where
    ContainerOf (a->a->a) = Matrix a
    ContainerOf (a->a) = Vector a
#endif

instance (Element a, Num a) => Build (a->a) where
    build' = buildV

instance (Element a, Num a) => Build (a->a->a) where
    build' = buildM

buildM :: (Integral a, Integral b, Num x, Num y, Element e) => (a, b) -> (x -> y -> e) -> Matrix e
buildM (rc,cc) f = fromLists [ [f r c | c <- cs] | r <- rs ]
    where rs = map fromIntegral [0 .. (rc-1)]
          cs = map fromIntegral [0 .. (cc-1)]

buildV :: (Integral a, Num b, Storable c) => a -> (b -> c) -> Vector c
buildV n f = fromList [f k | k <- ks]
    where ks = map fromIntegral [0 .. (n-1)]








-- type family BoundsOf x where

--     BoundsOf (a->a) = Integer
--     BoundsOf (a->a->a) = (Int,Int)


-- f :: (BoundsOf (a -> a) ~ Integer) => a
-- f = undefined
