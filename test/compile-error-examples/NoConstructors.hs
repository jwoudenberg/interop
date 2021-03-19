{-# LANGUAGE DeriveGeneric #-}

module NoConstructors () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data Type
  deriving (Generic)

instance Wire.Wire Type

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data 'False
--
--   I need a type to have at least one constructor.
--
-- • In the expression: $dmrec @(Type)
--   In an equation for ‘rec’: rec = $dmrec @(Type)
--   In the instance declaration for ‘Wire Type’
