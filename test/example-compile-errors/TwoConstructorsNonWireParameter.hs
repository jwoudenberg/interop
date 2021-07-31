{-# LANGUAGE DeriveGeneric #-}

module ExampleTypes.Either () where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data MyEither a b = MyLeft a | MyRight b
  deriving (Generic)

instance Wire.Wire (MyEither a b)

-- Compilation error:
--
-- • No instance for (WireG
--                      (Seq
--                         (Seq
--                            (EnsureRecordType
--                               "MyEither"
--                               'False
--                               'True
--                               "MyLeft"
--                               (S1
--                                  ('MetaSel
--                                     'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                                  (Rec0 a))
--                               (HasKindOfType a))
--                            (EnsureRecordType
--                               "MyEither"
--                               'True
--                               'False
--                               "MyRight"
--                               (S1
--                                  ('MetaSel
--                                     'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                                  (Rec0 b))
--                               (HasKindOfType b)))
--                         CustomType)
--                      (D1
--                         ('MetaData "MyEither" "ExampleTypes.Either" "main" 'False)
--                         (C1
--                            ('MetaCons "MyLeft" 'PrefixI 'False)
--                            (S1
--                               ('MetaSel
--                                  'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                               (Rec0 a))
--                          :+: C1
--                                ('MetaCons "MyRight" 'PrefixI 'False)
--                                (S1
--                                   ('MetaSel
--                                      'Nothing
--                                      'NoSourceUnpackedness
--                                      'NoSourceStrictness
--                                      'DecidedLazy)
--                                   (Rec0 b)))))
--     arising from a use of ‘$dmrec’
-- • In the expression: $dmrec @(MyEither a b)
--   In an equation for ‘rec’: rec = $dmrec @(MyEither a b)
--   In the instance declaration for ‘Wire (MyEither a b)’
