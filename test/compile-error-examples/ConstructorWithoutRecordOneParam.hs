{-# LANGUAGE DeriveGeneric #-}

module ConstructorWithoutRecordOneParam () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data ConstructorWithoutRecordOneParam = Constructor Int
  deriving (Generic)

instance Wire.Wire ConstructorWithoutRecordOneParam

-- Compilation error:
--
-- • Constructor parameter must be a record.
-- • In the expression: $dmrec @(ConstructorWithoutRecordOneParam)
--   In an equation for ‘rec’:
--       rec = $dmrec @(ConstructorWithoutRecordOneParam)
--   In the instance declaration for
--     ‘Wire ConstructorWithoutRecordOneParam’
