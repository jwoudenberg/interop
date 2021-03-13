{-# LANGUAGE DeriveGeneric #-}

module NonRecordParameter () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data Type = Constructor Int
  deriving (Generic)

instance Wire.Wire Type

-- Compilation error:
--
-- • No instance for (Generic Int) arising from a use of ‘$dmdecode’
-- • In the expression: $dmdecode @(Type)
--   In an equation for ‘decode’: decode = $dmdecode @(Type)
--   In the instance declaration for ‘Wire Type’
--
-- • No instance for (Generic Int) arising from a use of ‘$dmencode’
-- • In the expression: $dmencode @(Type)
--   In an equation for ‘encode’: encode = $dmencode @(Type)
--   In the instance declaration for ‘Wire Type’
--
-- • No instance for (Generic Int) arising from a use of ‘$dmtype_’
-- • In the expression: $dmtype_ @(Type)
--   In an equation for ‘type_’: type_ = $dmtype_ @(Type)
--   In the instance declaration for ‘Wire Type’
