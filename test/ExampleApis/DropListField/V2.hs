{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.DropListField.V2 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "DropListField" (\(req :: DropListFieldType) -> pure req)
  ]

data DropListFieldType = DropListFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire DropListFieldType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- No warnings.
