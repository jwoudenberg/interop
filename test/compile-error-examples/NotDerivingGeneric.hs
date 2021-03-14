{-# LANGUAGE DeriveGeneric #-}

module NotDerivingGeneric () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data NotDerivingGeneric = Constructor

instance Wire.Wire NotDerivingGeneric

-- Compilation error:
--
-- • No instance for (Generic NotDerivingGeneric)
--     arising from a use of ‘$dmrec’
-- • In the expression: $dmrec @(NotDerivingGeneric)
--   In an equation for ‘rec’: rec = $dmrec @(NotDerivingGeneric)
--   In the instance declaration for ‘Wire NotDerivingGeneric’
