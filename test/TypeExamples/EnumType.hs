{-# LANGUAGE OverloadedStrings #-}

module TypeExamples.EnumType (example, gen) where

import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Interop.Wire as Wire

data EnumType
  = OneConstructor
  | OtherConstructor
  deriving (Eq, Generic, Show)

instance Wire.Wire EnumType

example :: EnumType
example = OneConstructor

gen :: Hedgehog.Gen EnumType
gen = Gen.element [OneConstructor, OtherConstructor]

-- JSON encoding of example value:
--
-- {
--     "OneConstructor": {}
-- }
