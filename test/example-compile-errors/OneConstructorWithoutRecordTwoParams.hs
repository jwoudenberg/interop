{-# LANGUAGE DeriveGeneric #-}

module OneConstructorWithoutRecordTwoParams () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data OneConstructorWithoutRecordTwoParams = Constructor Int ()
  deriving (Generic)

instance Wire.Wire OneConstructorWithoutRecordTwoParams

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data OneConstructorWithoutRecordTwoParams
--       = Constructor Int ()
--
--   I'd like field names for all types used in constructors,
--   so you can make backwards-compatible changes to your types.
--   Try using record syntax:
--
--     data OneConstructorWithoutRecordTwoParams = Constructor
--       { x :: Int
--       , y :: ()
--       }
--
--   But come up with some better field names than x or y!
--
-- • In the expression: $dmrec @(OneConstructorWithoutRecordTwoParams)
--   In an equation for ‘rec’:
--       rec = $dmrec @(OneConstructorWithoutRecordTwoParams)
--   In the instance declaration for
--     ‘Wire OneConstructorWithoutRecordTwoParams’
