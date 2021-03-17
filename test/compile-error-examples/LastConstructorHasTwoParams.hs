{-# LANGUAGE DeriveGeneric #-}

module LastConstructorHasTwoParams () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data LastConstructorHasTwoParams
  = OneConstructor
  | Constructor Int ()
  deriving (Generic)

instance Wire.Wire LastConstructorHasTwoParams

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data LastConstructorHasTwoParams
--       = ...
--       | Constructor Int ()
--
--   I only support constructors with no parameters, or with a
--   a single parameter that must also be a record.
--   This is to make it easier for you to make changes to your
--   types in the future, in a backwards-compatible way.
--   Try creating a custom record type:
--
--     data LastConstructorHasTwoParams
--       = ...
--       | Constructor ConstructorRecord
--
--     data ConstructorRecord = ConstructorRecord
--       { x :: Int
--       , y :: ()
--       }
--
-- • In the expression: $dmrec @(LastConstructorHasTwoParams)
--   In an equation for ‘rec’:
--       rec = $dmrec @(LastConstructorHasTwoParams)
--   In the instance declaration for ‘Wire LastConstructorHasTwoParams’
