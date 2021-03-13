{-# LANGUAGE DeriveGeneric #-}

module MultipleConstructorParameters () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data Type = Constructor Int Int
  deriving (Generic)

instance Wire.Wire Type

-- Compilation error:
--
-- ../test/compile-error-examples/MultipleConstructorParameters.hs:11:10: error:
--     • Constructors with parameters need to use record syntax to have a 'Wire' instance.
--       This will allow you to add and change fields in backwards-compatible ways in the future.
--       Instead of:
--           data Coords = Coords Int Int
--       Try:
--           data Coords = Coords { x :: Int, y :: Int }
--     • In the expression: Interop.Wire.$dmtype_ @(Type)
--       In an equation for ‘Wire.type_’:
--           Wire.type_ = Interop.Wire.$dmtype_ @(Type)
--       In the instance declaration for ‘Wire.Wire Type’
--    |
-- 11 | instance Wire.Wire Type
--    |          ^^^^^^^^^^^^^^
--
-- ../test/compile-error-examples/MultipleConstructorParameters.hs:11:10: error:
--     • Constructors with parameters need to use record syntax to have a 'Wire' instance.
--       This will allow you to add and change fields in backwards-compatible ways in the future.
--       Instead of:
--           data Coords = Coords Int Int
--       Try:
--           data Coords = Coords { x :: Int, y :: Int }
--     • In the expression: Interop.Wire.$dmencode @(Type)
--       In an equation for ‘Wire.encode’:
--           Wire.encode = Interop.Wire.$dmencode @(Type)
--       In the instance declaration for ‘Wire.Wire Type’
--    |
-- 11 | instance Wire.Wire Type
--    |          ^^^^^^^^^^^^^^
--
-- ../test/compile-error-examples/MultipleConstructorParameters.hs:11:10: error:
--     • Constructors with parameters need to use record syntax to have a 'Wire' instance.
--       This will allow you to add and change fields in backwards-compatible ways in the future.
--       Instead of:
--           data Coords = Coords Int Int
--       Try:
--           data Coords = Coords { x :: Int, y :: Int }
--     • In the expression: Interop.Wire.$dmdecode @(Type)
--       In an equation for ‘Wire.decode’:
--           Wire.decode = Interop.Wire.$dmdecode @(Type)
--       In the instance declaration for ‘Wire.Wire Type’
--    |
-- 11 | instance Wire.Wire Type
--    |          ^^^^^^^^^^^^^^
