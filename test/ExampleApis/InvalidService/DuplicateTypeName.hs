{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleApis.InvalidService.DuplicateTypeName (endpoints) where

import qualified ExampleApis.InvalidService.ConflictingType as ConflictingType
import GHC.Generics (Generic)
import qualified Interop

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "public" (\() -> pure Train),
    Interop.endpoint "private" (\() -> pure ConflictingType.Cycle)
  ]

data Transportation
  = Bus
  | Tram
  | Train
  deriving (Generic)

instance Interop.Wire Transportation

-- Interop.service fails with:
--
-- The service uses two types with the same name:
--
--   ExampleApis.InvalidService.ConflictingType (Transportation)
--   ExampleApis.InvalidService.DuplicateTypeName (Transportation)
--
-- Try renaming one of the types.
--
-- Unique names avoid potential naming conflicts in generated client
-- code. There's many tricks I could play to avoid conflicts in
-- generated code, such as adding suffixes to dupplicate names or
-- generating multiple files if necessary. All of these make it harder
-- to understand how I work though, so I'd prefer not to.
