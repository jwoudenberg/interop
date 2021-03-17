{-# LANGUAGE DeriveGeneric #-}

module ConstructorWithoutRecordThreeParams () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data ConstructorWithoutRecordThreeParams = Constructor Int () Float
  deriving (Generic)

instance Wire.Wire ConstructorWithoutRecordThreeParams

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data ConstructorWithoutRecordThreeParams
--       = Constructor Int () ...
--
--   I'd like field names for all types used in constructors,
--   so you can make backwards-compatible changes to your types.
--   Try using record syntax:
--
--     data ConstructorWithoutRecordThreeParams = Constructor
--       { x :: Int
--       , y :: ()
--       , ...
--       }
--
--   But come up with some better field names than x or y!
--
-- • In the expression: $dmrec @(ConstructorWithoutRecordThreeParams)
--   In an equation for ‘rec’:
--       rec = $dmrec @(ConstructorWithoutRecordThreeParams)
--   In the instance declaration for
--     ‘Wire ConstructorWithoutRecordThreeParams’
