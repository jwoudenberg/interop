{-# LANGUAGE DeriveGeneric #-}

module ExampleTypes.Either () where

import GHC.Generics (Generic)
import qualified Interop

data MyEither a b = MyLeft a | MyRight b
  deriving (Generic)

instance Interop.Wire (MyEither a b)

-- Compilation error:
--
-- • I can't create a Wire instance for this type:
--
--     data MyEither
--       = MyLeft a
--       | ...
--
--   I only support constructors with no parameters, or with a
--   a single parameter that must also be a record.
--   This is to make it easier for you to make changes to your
--   types in the future, in a backwards-compatible way.
--   Try creating a custom record type:
--
--     data MyEither
--       = MyLeft MyLeftRecord
--       | ...
--
--     data MyLeftRecord = MyLeftRecord
--       { x :: a
--       }
--
-- • In the expression: $dmrec @(MyEither a b)
--   In an equation for ‘rec’: rec = $dmrec @(MyEither a b)
--   In the instance declaration for ‘Wire (MyEither a b)’
