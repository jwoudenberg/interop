{-# LANGUAGE DeriveGeneric #-}

module MultipleConstructorParameters () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data Type = Constructor Int Int
  deriving (Generic)

instance Wire.Wire Type

-- Compilation error:
--
-- • Constructors with parameters need to use record syntax to have a 'Wire' instance.
--   This will allow you to add and change fields in backwards-compatible ways in the future.
--   Instead of:
--       data Coords = Coords Int Int
--   Try:
--       data Coords = Coords { x :: Int, y :: Int }
-- • In the expression: $dmrec @(Type)
--   In an equation for ‘rec’: rec = $dmrec @(Type)
--   In the instance declaration for ‘Wire Type’
