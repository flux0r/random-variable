{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses #-}

import qualified Data.Foldable as F
import Data.Semigroup
import qualified Numeric.Natural as N

class F.Foldable f => SemiFoldable f where
    fold      :: Semigroup r => f r -> r
    fold_map  :: Semigroup a => (a -> r) -> f a -> r

class Multiplicative r where
    (*)     :: r -> r -> r
    (**)    :: N.Whole n => r -> n -> r
    product :: SemiFoldable f => (a -> r) -> f a -> r

class Additive r where
    (+)       :: r -> r -> r
    sinnum    :: N.Whole n => n -> r -> r
    sum       :: SemiFoldable f => (a -> r) -> f a -> r

class Additive r => Abelian r

class (Additive r, Abelian r, Multiplicative r) => Semiring r

class Semiring r => Algebra r a where
    mult :: (a -> a -> r) -> a -> r

class Multiplicative r => StarMultiplication r where
    adjoint :: r -> r

class (Semiring r, StarMultiplication r) => StarSemiring r

class (StarSemiring r, Algebra r a) => StarAlgebra r a where
    involution :: (a -> r) -> a -> r
