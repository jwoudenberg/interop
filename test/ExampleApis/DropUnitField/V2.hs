{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.DropUnitField.V2 where

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
  [ Interop.endpoint "DropUnitField" (\(req :: DropUnitFieldType) -> pure req)
  ]

data DropUnitFieldType = DropUnitFieldType
  { field :: Int
  }
  deriving (Generic)

instance Wire.Wire DropUnitFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
