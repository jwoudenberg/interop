{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddConstructor.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Interop
import qualified Interop.Wire as Wire

data AddConstructorType
  = FirstConstructor
  | SecondConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire AddConstructorType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddConstructor" (\(req :: AddConstructorType) -> pure req)
  ]

gen :: Hedgehog.Gen AddConstructorType
gen = Gen.element [FirstConstructor, SecondConstructor]
