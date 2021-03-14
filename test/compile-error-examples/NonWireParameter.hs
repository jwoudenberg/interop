{-# LANGUAGE DeriveGeneric #-}

module NonWireParameter () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data TypeWithNonWireParameter = Constructor NonWireType
  deriving (Generic)

instance Wire.Wire TypeWithNonWireParameter

data NonWireType

-- Compilation error:
--
-- • Constructor parameters of a type with a Wire instance must themselves have a Wire instance
-- • In the expression: $dmrec @(TypeWithNonWireParameter)
--   In an equation for ‘rec’: rec = $dmrec @(TypeWithNonWireParameter)
--   In the instance declaration for ‘Wire TypeWithNonWireParameter’
