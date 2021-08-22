{-# LANGUAGE DeriveGeneric #-}

module RecordWithBigTuple () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data RecordWithTuple = RecordWithTuple
  { fn :: (Int, Float, (), Int, Float, ())
  }
  deriving (Generic)

instance Wire.Wire RecordWithTuple

-- Compilation error:
--
-- • I'm trying to make a Wire instance of this type:
--
--     data RecordWithTuple = RecordWithTuple
--       { fn :: (Int, Float, (), Int, Float, ())
--       }
--
--   I prefer records over tuples, because those will allow you to make
--   backwards-compatible changes in the future.
--   Try using record syntax:
--
--     data RecordWithTuple = RecordWithTuple
--       { fn :: MyRecord
--       }
--
--     data MyRecord = MyRecord
--       { x :: Int
--       , y :: Float
--       , ...
--       }
--
--   But come up with some better field names than MyRecord, x, and y!
--
-- • In the expression: $dmrec @(RecordWithTuple)
--   In an equation for ‘rec’: rec = $dmrec @(RecordWithTuple)
--   In the instance declaration for ‘Wire RecordWithTuple’
