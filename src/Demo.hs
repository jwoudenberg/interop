module Demo where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text
-- import GHC.Generics (Generic)
import qualified Interop

data Person = Person Name Age

type Name = Text

type Age = Int

peeps :: Map.Map Int Person
peeps = Map.fromList [(0, Person "Jasper" 32)]

getPersonById :: Int -> IO (Maybe Person)
getPersonById personId = pure (Map.lookup personId peeps)

--  > Interop.generateRubyClient "person_service_v1.rb" ["PersonService"] Demo.service

-- Supporting boilerplate

mkService :: [Interop.Endpoint m] -> Interop.Service m
mkService endpoints =
  let serviceOrError = Interop.service endpoints
   in case serviceOrError of
        Left err -> error (Data.Text.unpack err)
        Right service' -> service'

-- service :: Interop.Service IO
-- service =
--   mkService
--     [ Interop.endpoint "get_person_by_id" getPersonById
--     ]
