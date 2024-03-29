{-# LANGUAGE DeriveGeneric #-}

module RecordWithFunction () where

import GHC.Generics (Generic)
import qualified Interop

data RecordWithFunction = RecordWithFunction
  { fn :: Int -> ()
  }
  deriving (Generic)

instance Interop.Wire RecordWithFunction

-- Compilation error:
--
-- • I'm trying to make a Wire instance of this type:
--
--     data RecordWithFunction = RecordWithFunction
--       { fn :: Int -> ()
--       }
--
--   I don't support functions in field types, because I don't know how to
--   encode them to JSON.
--
-- • In the expression: $dmrec @(RecordWithFunction)
--   In an equation for ‘rec’: rec = $dmrec @(RecordWithFunction)
--   In the instance declaration for ‘Wire RecordWithFunction’
