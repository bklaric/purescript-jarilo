module Test.Fetch where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Console (log)
import Jarilo.Fetch (requestUrl, requestUrlPath, requestUrlQuery')
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (type (:?), Mandatory, Many, Optional, Rest)
import Record (merge)
import Type.Proxy (Proxy(..))

type TestPath = Literal "games" :> Capture "handle" String

testPathParameters = { handle: "dota2" }

requestUrlPathTest :: String
requestUrlPathTest = requestUrlPath (Proxy :: _ TestPath) testPathParameters

type TestQuery = Optional "language" String :? Mandatory "page" Int :? Many "role" String :? Rest "other"

testQueryParameters = { language: Just "english", page: 3, role: ["leader", "player"], other: [Tuple "huehue" 123] }

requestUrlQueryTest :: String
requestUrlQueryTest = requestUrlQuery' (Proxy :: _ TestQuery) testQueryParameters

requestUrlTest = requestUrl (Proxy :: _ TestPath) (Proxy :: _ TestQuery) testPathParameters testQueryParameters

fetchTests = do
    log requestUrlPathTest
    log requestUrlQueryTest
    log requestUrlTest
