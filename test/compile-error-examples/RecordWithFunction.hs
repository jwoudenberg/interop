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
-- • No instance for (Wire (Int -> ())) arising from a use of ‘$dmrec’
--     (maybe you haven't applied a function to enough arguments?)
-- • In the expression: $dmrec @(RecordWithFunction)
--   In an equation for ‘rec’: rec = $dmrec @(RecordWithFunction)
--   In the instance declaration for ‘Wire RecordWithFunction’
