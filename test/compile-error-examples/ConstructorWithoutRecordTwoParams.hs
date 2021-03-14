{-# LANGUAGE DeriveGeneric #-}

module ConstructorWithoutRecordTwoParams () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data ConstructorWithoutRecordTwoParams = Constructor Int ()
  deriving (Generic)

instance Wire.Wire ConstructorWithoutRecordTwoParams

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data ConstructorWithoutRecordTwoParams = Constructor Int ()
--
--   I'd like field names for all types used in constructors,
--   so you can make backwards-compatible changes to your types.
--   Try using record syntax:
--
--     data ConstructorWithoutRecordTwoParams = Constructor
--       { x :: Int
--       , y :: ()
--       }
--
--   But come up with some better field names than x or y!
--
-- • In the expression: $dmrec @(ConstructorWithoutRecordTwoParams)
--   In an equation for ‘rec’:
--       rec = $dmrec @(ConstructorWithoutRecordTwoParams)
--   In the instance declaration for
--     ‘Wire ConstructorWithoutRecordTwoParams’
