{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddFirstFieldToSecondConstructor.V2 where

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
  [ Interop.endpoint "AddFirstFieldToSecondConstructor" (\(req :: AddFirstFieldToSecondConstructorType) -> pure req)
  ]

data AddFirstFieldToSecondConstructorType
  = AddFirstFieldFirstConstructor
  | AddFirstFieldSecondConstructor NewRecord
  deriving (Generic)

instance Wire.Wire AddFirstFieldToSecondConstructorType

data NewRecord = NewRecord
  { newField :: Maybe Int
  }
  deriving (Generic)

instance Wire.Wire NewRecord

-- Warnings when V1 is used by a server and V2 by a client:
--
-- No warnings.
