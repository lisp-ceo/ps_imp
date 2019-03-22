module Test.TuplesSpec where

import Prelude
import Math (sqrt)
import Test.Spec (Spec, describe, pending, it)
import Test.Spec.Assertions (shouldEqual)
import RayTracer.Tuples

spec :: Spec Unit
spec = do
  describe "Tuples" do
    describe "basic operations" do
      it "have a constructor that produces valid tuples" do
        let new_tuple = tuple 1.0 1.0 1.0 1.0
        new_tuple.x `shouldEqual` 1.0
      it "can also be constructed using record syntax" do
        let new_tuple = ({ x: 4.3, y: 4.2, z: 3.1, w: 1.0 } :: Tuple)
        new_tuple.x `shouldEqual` 4.3
        new_tuple.y `shouldEqual` 4.2
        new_tuple.z `shouldEqual` 3.1
        new_tuple.w `shouldEqual` 1.0
      it "isPoint returns true for a point" do
        let tuple = ({ x: 0.0, y: 0.0, z: 0.0, w: 1.0 } :: Tuple)
        isPoint tuple `shouldEqual` true
      it "isVector returns true for a vector" do
        let tuple = ({ x: 0.0, y: 0.0, z: 0.0, w: 0.0 } :: Tuple)
        isVector tuple `shouldEqual` true
      it "point() creates tuples with w=1" do
        let tuple = point 1.0 2.0 3.0
        isPoint tuple `shouldEqual` true
      it "vector() creates tuples with w=0" do
        let tuple = vector 1.0 2.0 3.0
        isVector tuple `shouldEqual` true
      it "adding two tuples" do
        let a1 = ({ x: 3.0, y: -2.0, z: 5.0, w: 1.0} :: Tuple)
        let a2 = ({ x: -2.0, y: 3.0, z: 1.0, w: 0.0} :: Tuple)
        (a1 + a2) `shouldEqual` ({ x: 1.0, y: 1.0, z: 6.0, w: 1.0} :: Tuple)
    -- TODO ps is strictly evaluated so result type is computed before testing
    -- see if theres a lazy evaluation package to use
      it "subtracing two points" do
        let a1 = point 3.0 2.0 1.0
        let a2 = point 5.0 6.0 7.0
        let result = vector (-2.0) (-4.0) (-6.0)
        (a1 - a2) `shouldEqual` result
      it "subtracting a vector from a point" do
        let a1 = point 3.0 2.0 1.0
        let a2 = vector 5.0 6.0 7.0
        let result = point (-2.0) (-4.0) (-6.0)
        (a1 - a2) `shouldEqual` result
      it "subtracting two vectors" do
        let v1 = vector 3.0 2.0 1.0
        let v2 = vector 5.0 6.0 7.0
        let result = vector (-2.0) (-4.0) (-6.0)
        (v1 - v2) `shouldEqual` result
      it "subtracing a vector from the zero vector" do
        let zero = vector 0.0 0.0 0.0
        let v = vector 1.0 (-2.0) 3.0
        let result = vector (-1.0) 2.0 (-3.0)
        (zero - v) `shouldEqual` result
      it "negating a tuple" do
        let a = tuple 1.0 (-2.0) 3.0 (-4.0)
        let result = tuple (-1.0) 2.0 (-3.0) 4.0
        (negate a) `shouldEqual` result
      it "multiplying a tuple by a scalar" do
        let a = tuple 1.0 (-2.0) (3.0) (-4.0)
        let result = tuple 3.5 (-7.0) 10.5 (-14.0)
        smult a 3.5 `shouldEqual` result
      it "multiplying a tuple by a fractionmult" do
        let a = tuple 1.0 (-2.0) (3.0) (-4.0)
        let result = tuple 0.5 (-1.0) 1.5 (-2.0)
        smult a 0.5 `shouldEqual` result
      it "dividing a tuple by a scalar" do
        let a = tuple 1.0 (-2.0) (3.0) (-4.0)
        let result = tuple 0.5 (-1.0) 1.5 (-2.0)
        sdiv a 2.0 `shouldEqual` result
    describe "magnitude" do
      it "computing the magnitude of vector(1.0, 0, 0)" do
        let v = vector 1.0 0.0 0.0
        magnitude v `shouldEqual` 1.0
      it "computing the magnitude of vector(0.0, 1, 0)" do
        let v = vector 0.0 1.0 0.0
        magnitude v `shouldEqual` 1.0
      it "computing the magnitude of vector(0.0, 1, 0)" do
        let v = vector 0.0 0.0 1.0
        magnitude v `shouldEqual` 1.0
      it "computing the magnitude of vector(1.0, 2.0, 3.0)" do
        let v = vector 1.0 2.0 3.0
        magnitude v `shouldEqual` sqrt(14.0)
      it "computing the magnitude of vector(-1.0, -2.0, -3.0)" do
        let v = vector (-1.0) (-2.0) (-3.0)
        magnitude v `shouldEqual` sqrt(14.0)
    describe "normalization" do
      it "normalizing vector(4.0, 0.0, 0.0) gives (1.0, 0.0, 0.0)" do
        let v = vector 4.0 0.0 0.0
        let result = vector 1.0 0.0 0.0
        normalize v `shouldEqual` result
      it "normalizing vector(1.0, 2.0, 3.0)" do
        let v = vector 1.0 2.0 3.0
        let result = etaCompare (normalize v) (vector 0.26726 0.53452 0.80178) 0.01
        result `shouldEqual` true
      it "the magnitude of a normalized vector" do
        let v = vector 1.0 2.0 3.0
        let norm = normalize v
        magnitude(norm) `shouldEqual` 1.0
    describe "dot product" do
      it "the dot product of two tuples" do
        let a = vector 1.0 2.0 3.0
        let b = vector 2.0 3.0 4.0
        dot a b `shouldEqual` 20.0
