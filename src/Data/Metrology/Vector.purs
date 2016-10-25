module Data.Metrology.Vector where

import Prelude
import Control.Comonad (extract)
import Data.Metrology (class ProductUnit, Quantity)
import Data.Matrix as M
import Data.Vector as V


-- | Multiply a scalar Quantity by a vector Quantity, multiplying the units.
scaleVQ ∷ ∀ t a u v w. (Field t, ProductUnit u v w) ⇒ Quantity u t → Quantity v (V.Vec a t) → Quantity w (V.Vec a t)
scaleVQ x v = pure (V.scale (extract x) (extract v))

-- | Multiply a scalar Quantity by a matrix Quantity, multiplying the units.
scaleMQ ∷ ∀ t a u v w. (Field t, ProductUnit u v w) ⇒ Quantity u t → Quantity v (M.Mat a t) → Quantity w (M.Mat a t)
scaleMQ x m = pure (M.scaleMatrix (extract x) (extract m))

-- Dot the vector value of two quantities and multiply the units.
dotQ ∷ ∀ a b c s. (ProductUnit a b c) ⇒ Quantity a (V.Vec s Number) → Quantity b (V.Vec s Number) → Quantity c Number
dotQ a b = pure (V.dot (extract a) (extract b))

-- | Calculate the square distance between the two points, multiplying the units.
distanceSquaredQ ∷ ∀ a b c s. ProductUnit a b c ⇒ Quantity a (V.Vec s Number) → Quantity b (V.Vec s Number) → Quantity c Number
distanceSquaredQ u v = pure (V.distanceSquared (extract u) (extract v))

-- | Calculate the length squared of a vector, squaring the unit.
vLengthSquaredQ ∷ ∀ u v s. ProductUnit u u v ⇒ Quantity u (V.Vec s Number) → Quantity v Number
vLengthSquaredQ x = (pure (V.vlengthSquared (extract x)))

