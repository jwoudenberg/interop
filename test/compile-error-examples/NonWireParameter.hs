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
-- • Before I can make a Wire instance for this type:
--
--     TypeWithNonWireParameter
--
--   I need Wire instances for all types used in its constructors,
--   but I'm missing a Wire instance for:
--
--     NonWireType
--
-- • In the expression: $dmrec @(TypeWithNonWireParameter)
--   In an equation for ‘rec’: rec = $dmrec @(TypeWithNonWireParameter)
--   In the instance declaration for ‘Wire TypeWithNonWireParameter’
