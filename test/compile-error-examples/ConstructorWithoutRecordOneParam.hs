{-# LANGUAGE DeriveGeneric #-}

module ConstructorWithoutRecordOneParam () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data ConstructorWithoutRecordOneParam = Constructor Int
  deriving (Generic)

instance Wire.Wire ConstructorWithoutRecordOneParam

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data ConstructorWithoutRecordOneParam
--       = Constructor Int
--
--   I'd like field names for all types used in constructors,
--   so you can make backwards-compatible changes to your types.
--   Try using record syntax:
--
--     data ConstructorWithoutRecordOneParam = Constructor
--       { x :: Int
--       }
--
--   But come up with some better field names than x or y!
--
-- • In the expression: $dmrec @(ConstructorWithoutRecordOneParam)
--   In an equation for ‘rec’:
--       rec = $dmrec @(ConstructorWithoutRecordOneParam)
--   In the instance declaration for
--     ‘Wire ConstructorWithoutRecordOneParam’
