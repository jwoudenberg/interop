{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddFirstField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Interop

data AddFirstFieldType
  = AddFirstField
  deriving (Generic, Eq, Show)

instance Interop.Wire AddFirstFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddFirstField" (\(req :: AddFirstFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen AddFirstFieldType
gen = pure AddFirstField
