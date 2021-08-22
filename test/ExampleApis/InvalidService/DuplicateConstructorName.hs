{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleApis.InvalidService.DuplicateConstructorName (endpoints) where

import qualified ExampleApis.InvalidService.ConflictingType as ConflictingType
import GHC.Generics (Generic)
import qualified Interop

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "annoying_animal" (\() -> pure Fly),
    Interop.endpoint "bad_for_environment" (\() -> pure ConflictingType.Fly)
  ]

data Animal
  = Otter
  | Elephant
  | Fly
  deriving (Generic)

instance Interop.Wire Animal

-- Interop.service fails with:
--
-- The service uses two constructors with the same name:
--
--   ExampleApis.InvalidService.DuplicateConstructorName (Animal(Fly))
--   ExampleApis.InvalidService.ConflictingType (Transportation(Fly))
--
-- Try renaming one of the constructors.
--
-- Unique names avoid potential naming conflicts in generated client
-- code. There's many tricks I could play to avoid conflicts in
-- generated code, such as adding suffixes to dupplicate names or
-- generating multiple files if necessary. All of these make it harder
-- to understand how I work though, so I'd prefer not to.
