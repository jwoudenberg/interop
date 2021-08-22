{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddHashSetField.V2 where

import Data.Function ((&))
import qualified Data.HashSet as HashSet
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddHashSetField" (\(req :: AddHashSetFieldType) -> pure req)
  ]

data AddHashSetFieldType = AddHashSetFieldType
  { field :: Int,
    otherHashSetField :: HashSet.HashSet Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddHashSetFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
