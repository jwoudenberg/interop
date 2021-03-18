{-# LANGUAGE DeriveGeneric #-}

module RecordWithFunction () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data RecordWithFunction = RecordWithFunction
  { fn :: Int -> ()
  }
  deriving (Generic)

instance Wire.Wire RecordWithFunction

-- Compilation error:
--
-- • I'm trying to make a Wire instance of this type:
--
--     data RecordWithFunction = RecordWithFunction
--       { fn :: Int -> ()
--       }
--
--   I need all the field types to have a Wire instance themselves,
--   but miss an instance for the type: Int -> ()
--
-- • In the expression: $dmrec @(RecordWithFunction)
--   In an equation for ‘rec’: rec = $dmrec @(RecordWithFunction)
--   In the instance declaration for ‘Wire RecordWithFunction’
