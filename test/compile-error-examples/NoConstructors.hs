{-# LANGUAGE DeriveGeneric #-}

module NoConstructors () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data Type
  deriving (Generic)

instance Wire.Wire Type

-- Compilation error:
--
-- • Type must have at least one constructor to have a 'Wire' instance.
-- • In the expression: $dmrec @(Type)
--   In an equation for ‘rec’: rec = $dmrec @(Type)
--   In the instance declaration for ‘Wire Type’
