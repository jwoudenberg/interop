{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleTypes.EnumType (example, gen) where

import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Interop

data EnumType
  = OneConstructor
  | OtherConstructor
  deriving (Eq, Generic, Show)

instance Interop.Wire EnumType

example :: EnumType
example = OneConstructor

gen :: Hedgehog.Gen EnumType
gen = Gen.element [OneConstructor, OtherConstructor]

-- JSON encoding of example value:
--
-- {
--     "OneConstructor": {}
-- }
