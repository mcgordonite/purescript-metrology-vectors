module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Matrix (fromArray, Mat) as M
import Data.Metrology (class ProductUnit, Quantity)
import Data.Metrology.Vector (distanceSquaredQ, dotQ, scaleMQ, scaleVQ, vLengthSquaredQ)
import Data.TypeNat (Two)
import Data.Vector (distanceSquared, dot, vlengthSquared) as V
import Data.Vector3 (vec3) as V
import Test.QuickCheck ((===))
import Test.Unit (suite, test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)


data KittenUnit
type Kittens = Quantity KittenUnit

data MinuteUnit
type Minutes = Quantity MinuteUnit

data KittenPerMinuteUnit
type KittensPerMinute = Quantity KittenPerMinuteUnit

instance productUnitKittenPerMinute ∷ ProductUnit MinuteUnit KittenPerMinuteUnit KittenUnit
instance inverseProductUnitKittenPerMinute ∷ ProductUnit KittenPerMinuteUnit MinuteUnit KittenUnit

data SquareKittenUnit
type SquareKittens = Quantity SquareKittenUnit

instance productUnitSquareKitten ∷ ProductUnit KittenUnit KittenUnit SquareKittenUnit

kittens ∷ ∀ t. t → Kittens t
kittens = pure

minutes ∷ ∀ t. t → Minutes t
minutes = pure

numberMinutes ∷ Number → Minutes Number
numberMinutes = pure

kittensPerMinute ∷ ∀ t. t → KittensPerMinute t
kittensPerMinute = pure

squareKittens ∷ ∀ t. t → SquareKittens t
squareKittens = pure

mat2 ∷ ∀ t. t → t → t → t → M.Mat Two t
mat2 a b c d = M.fromArray [a, b, c, d]


main ∷ Eff (avar ∷ AVAR, console ∷ CONSOLE, random ∷ RANDOM, testOutput ∷ TESTOUTPUT) Unit
main = runTest do
  suite "Data.Metrology.Vector" do
    test "scaleVQ" do
      quickCheck \x y z n → scaleVQ (numberMinutes n) (kittensPerMinute (V.vec3 x y z)) === kittens (V.vec3 (n * x) (n * y) (n * z))
    test "scaleMQ" do
      quickCheck \a b c d n → scaleMQ (numberMinutes n) (kittensPerMinute (mat2 a b c d)) === kittens (mat2 (n * a) (n * b) (n * c) (n * d))
    test "dotQ" do
      quickCheck \a b c x y z →
        let
	  v1 = V.vec3 a b c
          v2 = V.vec3 x y z
	in dotQ (kittensPerMinute v1) (minutes v2) === kittens (V.dot v1 v2 ∷ Number)
    test "distanceSquared" do
      quickCheck \a b c x y z →
        let
          v1 = V.vec3 a b c
          v2 = V.vec3 x y z
        in distanceSquaredQ (minutes v1) (kittensPerMinute v2) === kittens (V.distanceSquared v1 v2)
    test "vLengthSquaredQ" do
      quickCheck \x y z → 
        let v = V.vec3 x y z
        in vLengthSquaredQ (kittens v) === squareKittens (V.vlengthSquared v)

