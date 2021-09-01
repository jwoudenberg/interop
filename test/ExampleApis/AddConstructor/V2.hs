{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddConstructor.V2 where

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
  [ Interop.endpoint "AddConstructor" (\(req :: AddConstructorType) -> pure req)
  ]

data AddConstructorType
  = FirstConstructor
  | SecondConstructor
  | ThirdConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire AddConstructorType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- The server might return the 'ThirdConstructor' constructor of the type 'AddConstructorType', but the generated client code doesn't know that constructor.
-- data AddConstructorType = ThirdConstructor
--
-- Maybe you're trying to add a new constructor to a type? If so, make sure to follow these steps:
--
-- 1. Add the constructor to the type but make sure not to use it in server responses yet.
-- 2. Change the client to support the new constructor.
-- 3. Make sure the changes from step 1 and 2 are deployed.
-- 4. Now you can start using the constructor in server code!
--
-- If you're currently at step 1 then this warning is expected.
