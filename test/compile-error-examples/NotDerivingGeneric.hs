{-# LANGUAGE DeriveGeneric #-}

module NotDerivingGeneric () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data NotDerivingGeneric = Constructor

instance Wire.Wire NotDerivingGeneric

-- Compilation error:
--
-- • Missing Generic instance.
-- • In the expression: $dmrec @(NotDerivingGeneric)
--   In an equation for ‘rec’: rec = $dmrec @(NotDerivingGeneric)
--   In the instance declaration for ‘Wire NotDerivingGeneric’
