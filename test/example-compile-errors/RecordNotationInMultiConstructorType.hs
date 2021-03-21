{-# LANGUAGE DeriveGeneric #-}

module RecordNotationInMultiConstructorType () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data RecordNotationInMultiConstructorType
  = OneConstructor
  | Constructor {int :: Int, tuple :: ()}
  | ThreeConstructor
  deriving (Generic)

instance Wire.Wire RecordNotationInMultiConstructorType

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data RecordNotationInMultiConstructorType
--       = ...
--       | Constructor
--           { int :: Int
--           , tuple :: ()
--           }
--       | ...
--
--   I only support constructors with no parameters, or with a
--   a single parameter that must a separate record type.
--   This is to make it easier for you to make changes to your
--   types in the future, in a backwards-compatible way.
--   Try creating a custom record type:
--
--     data RecordNotationInMultiConstructorType
--       = ...
--       | Constructor ConstructorRecord
--       | ...
--
--     data ConstructorRecord = ConstructorRecord
--       { int :: Int
--       , tuple :: ()
--       }
--
-- • In the expression: $dmrec @(RecordNotationInMultiConstructorType)
--   In an equation for ‘rec’:
--       rec = $dmrec @(RecordNotationInMultiConstructorType)
--   In the instance declaration for
--     ‘Wire RecordNotationInMultiConstructorType’
