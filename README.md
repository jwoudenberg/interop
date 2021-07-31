# Interop

This library is a work in progress.

An RPC implementation for Haskell servers with clients in other languages. Supports Ruby code generation with Sorbet type annotations, with plans to add support for Elm code generation.

Running code and test watcher:

```sh
ghcid --command 'cabal repl test:tests' --test Main.main
```

## Example

```hs
module Service (main) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics (Generic)
import qualified Interop
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  let endpoints =
        [ Interop.endpoint "get_person_by_id" getPersonById
        ]
  service <-
    case Interop.service endpoints of
          Left err -> fail (Data.Text.unpack err)
          Right service' -> pure service'
  Interop.generateRubyClient
    "person_service_v2.rb"
    ["PersonService"]
    service
  Warp.serve 9000 (Interop.wai service)

data Person = Person {name :: Text}
  deriving (Generic)

instance Interop.Wire Person

peeps :: Map.Map Int Person
peeps = Map.fromList [(0, Person "Jasper")]

getPersonById :: Int -> IO (Maybe Person)
getPersonById personId = pure (Map.lookup personId peeps)
```

## Comparison with similar tools

A lot of tools exist for these kinds of integrations. As far as I'm aware these library fills a new spot in the design space. Here's some comparisons with other approaches I'm aware off.

### Code generation for Servant APIs

The [servant][] library for Haskell can be used to describe APIs using types. Many supporting libraries exist to generate client code from these types, for example [servant-elm][], [elm-bridge][], [servant-ruby][], and more.

This library's approach differs in the following ways:

- `interop` only supports RPC-style endpoints with a request type and a response type. This allows the API of this library to be significantly smaller than Servant's, which aims to support all of HTTP.
- `interop` is opinionated and does not offer many configuration options. To generate client code we can call a single function that will produce in a single file functions for making HTTP requests, client versions of Haskell types used in the API, and encoders/decoders for those types.
- `interop` aims to have very good error messages that guide users through the steps they need to take to turn their regular Haskell functions into endpoints that can be called from clients. It can also warn when changes in the server's API might not be compatible with existing clients.

### RPC implementations around generic schema languages

There's multiple language-agnostic schema definition languages with RPC support, aimed at observing contracts between services potentially written in different languages. Examples include [protocol buffers][] and [cap'n proto][].

This libary's approach differs in the following ways:

- `interop` is not language-agnostic. It assumes a Haskell server and so is significantly less flexible. The upside of this decision is that running `interop` requires little knowledge beyond basic familiarity with Haskell and the language you generate client code for.
- My impression of the RPC solutions linked above is that they are aimed at providing long-term backwards-compatibility in APIs. This makes sense for teams that create APIs consumed by other teams or external customers. I've found while working in smaller organizations where the team owns both client and server users of an API, that we had less need of a tool that discouraged breaking changes entirely, over one that warned us for inadvertent breaking changes and supported a safe roll-out of intentional ones. `interop` aims to support that latter use-case.
- Although language-agnostic, I feel that the RPS solutions linked above object-oriented roots in the way that they model data. This means describing Haskell or Elm-like ADTs in the schema languages of these tools requires some messaging. `interop` uses Haskell types as a 'schema' and so doesn't have this problem.

[servant]: https://haskell-servant.github.io/
[elm-bridge]: https://github.com/agrafix/elm-bridge
[servant-elm]: https://github.com/haskell-servant/servant-elm
[servant-ruby]: https://hackage.haskell.org/package/servant-ruby
[protocol buffers]: https://developers.google.com/protocol-buffers/
[cap'n proto]: https://capnproto.org/
