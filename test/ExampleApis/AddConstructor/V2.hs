{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddConstructor.V2 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddConstructor" (\(req :: AddConstructorType) -> pure req)
  ]

data AddConstructorType
  = FirstConstructor
  | SecondConstructor
  | ThirdConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire AddConstructorType

-- Warnings for this change from Base type:
--
-- A constructor was added to a type used in responses.
-- data AddConstructorType = ThirdConstructor
--
-- Using this constructor in responses will cause failures in versions of clients that do not support it yet. Make sure to upgrade those clients before using the new constructor!
