{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.RemoveConstructor.V2 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "RemoveConstructor" (\(req :: RemoveConstructorType) -> pure req)
  ]

data RemoveConstructorType
  = KeepThisConstructor
  | AlsoKeepThisConstructor
  deriving (Generic)

instance Interop.Wire RemoveConstructorType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- The generated client code supports use the 'RemoveThisConstructor' constructor of the type 'RemoveConstructorType' in requests, but the server doesn't know this constructor
-- data RemoveConstructorType = RemoveThisConstructor
--
-- Maybe you're trying to remove a constructor from a type? If so, make sure to follow these steps:
--
-- 1. Stop using the constructor in server code.
-- 2. Make sure the changes from step 1 are deployed.
-- 3. Remove the constructor from the server code.
--
-- If you're currently at step 1 then this warning is expected.
