module Test.TuplesSpec where

import Prelude
import RayTracer.Tuples(Tuple, tuple, point)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Tuples" do
    describe "record" do
      it "has a constructor that produces valid tuples" do
        let new_tuple = tuple 1.0 1.0 1.0 1.0
        new_tuple.x `shouldEqual` 1.0
      it "tuples can also be constructed using record syntax" do
        let new_tuple = ({ x: 4.3, y: 4.2, z: 3.1, w: 1.0 } :: Tuple)
        new_tuple.x `shouldEqual` 4.3
        new_tuple.y `shouldEqual` 4.2
        new_tuple.z `shouldEqual` 3.1       
        new_tuple.w `shouldEqual` 1.0
    describe "point" do
      it "returns true for a point" do
        let tuple = ({ x: 0.0, y: 0.0, z: 0.0, w: 0.0 } :: Tuple)
        point tuple `shouldEqual` true
