{-# LANGUAGE DeriveGeneric #-}

module NonRecordParameter () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data Type = Constructor Int
  deriving (Generic)

instance Wire.Wire Type

-- Compilation error:
--
-- • Constructor parameter must be a record.
-- • In the expression: $dmrec @(Type)
--   In an equation for ‘rec’: rec = $dmrec @(Type)
--   In the instance declaration for ‘Wire Type’
