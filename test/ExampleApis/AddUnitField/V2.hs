{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddUnitField.V2 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Interop

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

instance Interop.Wire AddUnitFieldType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- No warnings.
