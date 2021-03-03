/2f2f
client code in multiple languages.

## Goals

- A small API for defining RPC functions in Haskell, using sensible defaults and few configuration options.
- Serialization and deserialization requires no hand-written code (in fact does not allow it).
- Support a wide variety of Haskell types, _except_ types that cannot be changed in backwards-compatible fashion. In those cases provide suggestions for to change the type.
- Quality error messages
- Generate Ruby client code with Sorbet type annotations
- Generate Elm client code.
- Ability to compare different versions of APIs, tell when a change is backwards incompatible, and provide suggestions to make a backwards compatible change instead.

## API Design Napkin

```hs
import qualified Interop

service :: Interop.Service IO
service =
  mconcat
    [ Interop.endpoint (\(Add x y) -> pure (x + y))
    , Interop.endpoint (\(Multiply x y) -> pure (x * y))
    ]

-- Explanation: Each `Interop.endpoint` takes a function `req -> m res`.
-- The `req` type must be unique and it serves as the name of the endpoints in
-- generated code.

data Add Int Int
  deriving (Generic)

instance Interop.Wire Add

data Multiply Int Int
  deriving (Generic)

instance Interop.Wire Add

main :: IO ()
main = do
  Interop.generateRuby "ruby/lib/generated/api.rb" service
  Interop.generateElm "elm/src/Generated/Api.elm" service
  Warp.serve 9000 (Interop.toWaiApplication service)
```

## Maybe some day: binary data format

A binary data format would be nice to improve performance. It's not an immediate priority though because it will likely make implementing the main goals of the library harder. I'd love to have this in some future version though.
