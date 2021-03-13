{-# LANGUAGE DeriveGeneric #-}

module NonRecordParameter () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data Type = Constructor Int
  deriving (Generic)

instance Wire.Wire Type

-- Compilation error:
--
-- ../test/compile-error-examples/NonRecordParameter.hs:11:10: error:
--     • No instance for (Generic Int)
--         arising from a use of ‘Interop.Wire.$dmtype_’
--     • In the expression: Interop.Wire.$dmtype_ @(Type)
--       In an equation for ‘Wire.type_’:
--           Wire.type_ = Interop.Wire.$dmtype_ @(Type)
--       In the instance declaration for ‘Wire.Wire Type’
--    |
-- 11 | instance Wire.Wire Type
--    |          ^^^^^^^^^^^^^^
--
-- ../test/compile-error-examples/NonRecordParameter.hs:11:10: error:
--     • No instance for (Generic Int)
--         arising from a use of ‘Interop.Wire.$dmencode’
--     • In the expression: Interop.Wire.$dmencode @(Type)
--       In an equation for ‘Wire.encode’:
--           Wire.encode = Interop.Wire.$dmencode @(Type)
--       In the instance declaration for ‘Wire.Wire Type’
--    |
-- 11 | instance Wire.Wire Type
--    |          ^^^^^^^^^^^^^^
--
-- ../test/compile-error-examples/NonRecordParameter.hs:11:10: error:
--     • No instance for (Generic Int)
--         arising from a use of ‘Interop.Wire.$dmdecode’
--     • In the expression: Interop.Wire.$dmdecode @(Type)
--       In an equation for ‘Wire.decode’:
--           Wire.decode = Interop.Wire.$dmdecode @(Type)
--       In the instance declaration for ‘Wire.Wire Type’
--    |
-- 11 | instance Wire.Wire Type
--    |          ^^^^^^^^^^^^^^
