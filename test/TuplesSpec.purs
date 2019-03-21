module Test.TuplesSpec where

import Prelude
import Test.Spec (Spec, describe, pending, it)
import Test.Spec.Assertions (shouldEqual)
import RayTracer.Tuples

spec :: Spec Unit
spec = do
  describe "Tuples" do
    -- describe "Vector" do
    --   it "constructs Vecotr objects" do
    --     let vector = Vector 4.0 5.0 6.0
    --     true `shouldEqual` true
    -- describe "Point" do
    --   it "constructs Point objects" do
    --     let point = Point 1.0 2.0 3.0
    --     true `shouldEqual` true
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
    describe "isPoint" do
      it "returns true for a point" do
        let tuple = ({ x: 0.0, y: 0.0, z: 0.0, w: 1.0 } :: Tuple)
        isPoint tuple `shouldEqual` true
    describe "isVector" do
      it "returns true for a vector" do
        let tuple = ({ x: 0.0, y: 0.0, z: 0.0, w: 0.0 } :: Tuple)
        isVector tuple `shouldEqual` true
    describe "point() creates tuples with w=1" do
      it "uses the point construct to create valid points" do
        let tuple = point 1.0 2.0 3.0
        isPoint tuple `shouldEqual` true
    describe "vector() creates tuples with w=0" do
      it "uses the vector construct to create valid vectors" do
        let tuple = vector 1.0 2.0 3.0
        isVector tuple `shouldEqual` true
    describe "adding two tuples" do
      it "adds tuples correctly" do
        let a1 = ({ x: 3.0, y: -2.0, z: 5.0, w: 1.0} :: Tuple)
        let a2 = ({ x: -2.0, y: 3.0, z: 1.0, w: 0.0} :: Tuple)
        (a1 + a2) `shouldEqual` ({ x: 1.0, y: 1.0, z: 6.0, w: 1.0} :: Tuple)
