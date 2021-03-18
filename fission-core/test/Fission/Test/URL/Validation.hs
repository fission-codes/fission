module Fission.Test.URL.Validation (spec) where

import           Fission.URL.Validation

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "URL.Validation" $ parallel do
    describe "isValid" do
      it "is valid on a simple string" do
        isValid "simple" `shouldBe` True

      -- it "" do



-- >>> isValid "happy-name"
-- True
--
-- >>> isValid "under_score"
-- True
--
-- Blocklisted words are not allowed
--
-- >>> isValid "recovery"
-- False
--
-- They're not case sensitive
--
-- >>> isValid "reCovErY"
-- False
--
-- They can't contain uppercase characters at all
--
-- >>> isValid "hElLoWoRlD"
-- False
--
-- Nor are various characters
--
-- >>> isValid "plus+plus"
-- False
--
-- >>> isValid "-startswith"
-- False
--
-- >>> isValid "endswith-"
-- False
--
-- >>> isValid "_startswith"
-- False
--
-- >>> isValid "with.space"
-- False
--
-- >>> isValid "with.dot"
-- False
--
-- >>> isValid "has.two.dots"
-- False
--
-- >>> isValid "name&with#chars"
-- False
