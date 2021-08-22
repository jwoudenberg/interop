{-# LANGUAGE DeriveGeneric #-}

module NoConstructors () where

import GHC.Generics (Generic)
import qualified Interop

data TypeWithoutConstructors
  deriving (Generic)

instance Interop.Wire TypeWithoutConstructors

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data TypeWithoutConstructors
--
--   I need a type to have at least one constructor.
--
-- • In the expression: $dmrec @(TypeWithoutConstructors)
--   In an equation for ‘rec’: rec = $dmrec @(TypeWithoutConstructors)
--   In the instance declaration for ‘Wire TypeWithoutConstructors’
