module Domain.ValidationSpec where

import ClassyPrelude

import Test.Hspec
import Text.Regex.PCRE.Heavy

import Domain.Validation

spec :: Spec
spec = do
  describe "rangeBetween" $ do
    let validator = rangeBetween 1 10 "fail"
    it "val < min should fail" $
      validator 0 `shouldBe` Just "fail"
    it "val == min should pass" $
      validator 1 `shouldBe` Nothing
    it "min < val < max should pass" $
      validator 5 `shouldBe` Nothing
    it "val == max should pass" $
      validator 10 `shouldBe` Nothing
    it "val > max should fail" $
      validator 11 `shouldBe` Just "fail"

  describe "lengthBetween" $ do
    let validator = lengthBetween 1 10 "fail"
    it "val < min should fail" $
      validator [] `shouldBe` Just "fail"
    it "val == min should pass" $
      validator [1] `shouldBe` Nothing
    it "min < val < max should pass" $
      validator [1..5] `shouldBe` Nothing
    it "val == max should pass" $
      validator [1..10] `shouldBe` Nothing
    it "val > max should fail" $
      validator [1..11] `shouldBe` Just "fail"

  describe "regexMatches" $ do
    let validator = regexMatches [re|^hello|] "fail"
    it "if matches found then it should pass" $
      validator "hello world" `shouldBe` Nothing
    it "if no matches found then it should fail" $
      validator "world hello" `shouldBe` Just "fail"

