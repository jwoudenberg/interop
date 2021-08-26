{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddUnitField.V2 where

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
  [ Interop.endpoint "AddUnitField" (\(req :: AddUnitFieldType) -> pure req)
  ]

data AddUnitFieldType = AddUnitFieldType
  { field :: Int,
    newUnitField :: ()
  }
  deriving (Generic)

instance Wire.Wire AddUnitFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
