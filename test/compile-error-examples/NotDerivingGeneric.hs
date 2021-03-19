{-# LANGUAGE DeriveGeneric #-}

module NotDerivingGeneric () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data NotDerivingGeneric = Constructor

instance Wire.Wire NotDerivingGeneric

-- Compilation error:
--
-- • I'm trying to make a wire instance for this type:
--
--     data NotDerivingGeneric = ...
--
--   I need a Generic instance of this type to learn more about it.
--   Add one like this:
--
--     data NotDerivingGeneric = ...
--       deriving (Generic)
--
-- • In the expression: $dmrec @(NotDerivingGeneric)
--   In an equation for ‘rec’: rec = $dmrec @(NotDerivingGeneric)
--   In the instance declaration for ‘Wire NotDerivingGeneric’
