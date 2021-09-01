{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddSetField.V2 where

import Data.Function ((&))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddSetField" (\(req :: AddSetFieldType) -> pure req)
  ]

data AddSetFieldType = AddSetFieldType
  { field :: Int,
    otherSetField :: Set.Set Int
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire AddSetFieldType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- No warnings.
