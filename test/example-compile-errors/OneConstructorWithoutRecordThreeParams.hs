{-# LANGUAGE DeriveGeneric #-}

module OneConstructorWithoutRecordThreeParams () where

import GHC.Generics (Generic)
import qualified Interop

data OneConstructorWithoutRecordThreeParams = Constructor Int () Float
  deriving (Generic)

instance Interop.Wire OneConstructorWithoutRecordThreeParams

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data OneConstructorWithoutRecordThreeParams
--       = Constructor Int () ...
--
--   I'd like field names for all types used in constructors,
--   so you can make backwards-compatible changes to your types.
--   Try using record syntax:
--
--     data OneConstructorWithoutRecordThreeParams = Constructor
--       { x :: Int
--       , y :: ()
--       , ...
--       }
--
--   But come up with some better field names than x or y!
--
-- • In the expression:
--     $dmrec @(OneConstructorWithoutRecordThreeParams)
--   In an equation for ‘rec’:
--       rec = $dmrec @(OneConstructorWithoutRecordThreeParams)
--   In the instance declaration for
--     ‘Wire OneConstructorWithoutRecordThreeParams’
