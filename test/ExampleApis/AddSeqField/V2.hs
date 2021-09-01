{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddSeqField.V2 where

import Data.Function ((&))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddSeqField" (\(req :: AddSeqFieldType) -> pure req)
  ]

data AddSeqFieldType = AddSeqFieldType
  { field :: Int,
    otherSeqField :: Seq.Seq Int
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire AddSeqFieldType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- No warnings.
