{-# LANGUAGE DeriveGeneric #-}

module TwoConstructorsWithoutRecordOneParam () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data TwoConstructorsWithoutRecordOneParam
  = OneConstructor
  | TwoConstructor Int
  deriving (Generic)

instance Wire.Wire TwoConstructorsWithoutRecordOneParam

-- Compilation error:
--
-- • No instance for (Generic Int) arising from a use of ‘$dmrec’
-- • In the expression: $dmrec @(TwoConstructorsWithoutRecordOneParam)
--   In an equation for ‘rec’:
--       rec = $dmrec @(TwoConstructorsWithoutRecordOneParam)
--   In the instance declaration for
--     ‘Wire TwoConstructorsWithoutRecordOneParam’
