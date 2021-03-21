{-# LANGUAGE OverloadedStrings #-}

module ExampleTypes.RecursiveType (example, gen) where

import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Interop.Wire as Wire

data RecursiveType = RecursiveType
  { recursiveField :: Maybe RecursiveType
  }
  deriving (Eq, Generic, Show)

instance Wire.Wire RecursiveType

example :: RecursiveType
example = RecursiveType (Just (RecursiveType Nothing))

gen :: Hedgehog.Gen RecursiveType
gen =
  RecursiveType
    <$> Gen.recursive
      Gen.choice
      [pure Nothing]
      [Just <$> gen]

-- JSON encoding of example value:
--
-- {
--     "recursiveField": {
--         "recursiveField": null
--     }
-- }
