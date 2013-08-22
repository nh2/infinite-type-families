{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test where


type family BoundsOf x

type instance BoundsOf (a->a) = Int
type instance BoundsOf (a->a->a) = (Int,Int)


type family G a

type instance G Int = G Int -> G Int
