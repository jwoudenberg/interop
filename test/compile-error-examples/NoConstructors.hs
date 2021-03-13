{-# LANGUAGE DeriveGeneric #-}

module NoConstructors () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data Type
  deriving (Generic)

instance Wire.Wire Type

-- ../test/compile-error-examples/NoConstructors.hs:11:10: error:
--     • Type must have at least one constructor to have a 'Wire' instance.
--     • In the expression: Interop.Wire.$dmtype_ @(Type)
--       In an equation for ‘Wire.type_’:
--           Wire.type_ = Interop.Wire.$dmtype_ @(Type)
--       In the instance declaration for ‘Wire.Wire Type’
--    |
-- 11 | instance Wire.Wire Type
--    |          ^^^^^^^^^^^^^^
--
-- ../test/compile-error-examples/NoConstructors.hs:11:10: error:
--     • Type must have at least one constructor to have a 'Wire' instance.
--     • In the expression: Interop.Wire.$dmencode @(Type)
--       In an equation for ‘Wire.encode’:
--           Wire.encode = Interop.Wire.$dmencode @(Type)
--       In the instance declaration for ‘Wire.Wire Type’
--    |
-- 11 | instance Wire.Wire Type
--    |          ^^^^^^^^^^^^^^
--
-- ../test/compile-error-examples/NoConstructors.hs:11:10: error:
--     • Type must have at least one constructor to have a 'Wire' instance.
--     • In the expression: Interop.Wire.$dmdecode @(Type)
--       In an equation for ‘Wire.decode’:
--           Wire.decode = Interop.Wire.$dmdecode @(Type)
--       In the instance declaration for ‘Wire.Wire Type’
--    |
-- 11 | instance Wire.Wire Type
--    |          ^^^^^^^^^^^^^^
