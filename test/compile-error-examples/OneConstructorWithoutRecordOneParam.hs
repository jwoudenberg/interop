{-# LANGUAGE DeriveGeneric #-}

module OneConstructorWithoutRecordOneParam () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data OneConstructorWithoutRecordOneParam = Constructor Int
  deriving (Generic)

instance Wire.Wire OneConstructorWithoutRecordOneParam

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data OneConstructorWithoutRecordOneParam
--       = Constructor Int
--
--   I'd like field names for all types used in constructors,
--   so you can make backwards-compatible changes to your types.
--   Try using record syntax:
--
--     data OneConstructorWithoutRecordOneParam = Constructor
--       { x :: Int
--       }
--
--   But come up with some better field names than x or y!
--
-- • In the expression: $dmrec @(OneConstructorWithoutRecordOneParam)
--   In an equation for ‘rec’:
--       rec = $dmrec @(OneConstructorWithoutRecordOneParam)
--   In the instance declaration for
--     ‘Wire OneConstructorWithoutRecordOneParam’
