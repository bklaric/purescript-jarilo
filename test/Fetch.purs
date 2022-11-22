module Test.Fetch where

import Prelude

import Async (Async, runAsync)
import Control.Monad.Except (except)
import Data.Bifunctor (lmap)
import Data.Either (Either, either, note)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Foreign (ForeignError(..), readString)
import Jarilo.Fetch (defaultOptions, fetch, requestUrl, requestUrlPath, requestUrlQuery')
import Jarilo.Method (Get)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (type (:?), Mandatory, Many, Optional, Rest)
import Jarilo.Response (type (:!), BadRequest, NoContent, Ok)
import Jarilo.Route (FullRoute)
import Jarilo.Shared.Component (class Component)
import Record (merge)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Yoga.JSON (class ReadForeign)

type TestPath = Literal "games" :> Capture "handle" String :> Literal "players"

testPathParameters = { handle: "valorant" }

requestUrlPathTest :: String
requestUrlPathTest = requestUrlPath (Proxy :: _ TestPath) testPathParameters

type TestQuery = Optional "microphone" Boolean :? Mandatory "page" Int :? Mandatory "timezone" String :? Many "location" String :? Rest "fields"

testQueryParameters = { microphone: Just true, page: 1, timezone: "Europe/Zagreb", location: ["Europe"], fields: [Tuple "rank" "silver", Tuple "rank" "gold"] }

requestUrlQueryTest :: String
requestUrlQueryTest = requestUrlQuery' (Proxy :: _ TestQuery) testQueryParameters

requestUrlTest = requestUrl (Proxy :: _ TestPath) (Proxy :: _ TestQuery) testPathParameters testQueryParameters

data Platform = Steam | Riot | BattleNet | Origin | Ubisoft | PlayStation | Xbox | Switch

fromString :: String -> Maybe Platform
fromString "steam" = Just Steam
fromString "riot" = Just Riot
fromString "battle.net" = Just BattleNet
fromString "origin" = Just Origin
fromString "ubisoft-connect" = Just Ubisoft
fromString "playstation" = Just PlayStation
fromString "xbox" = Just Xbox
fromString "switch" = Just Switch
fromString _ = Nothing

fromString' :: String -> Either String Platform
fromString' platform = fromString platform # note ("Unknown platform: " <> platform)

toString :: Platform -> String
toString Steam = "steam"
toString Riot = "riot"
toString BattleNet = "battle.net"
toString Origin = "origin"
toString Ubisoft = "ubisoft-connect"
toString PlayStation = "playstation"
toString Xbox = "xbox"
toString Switch = "switch"

instance Component Platform where
    toComponent = toString
    fromComponent = fromString'

instance ReadForeign Platform where
    readImpl platform' =
        readString platform'
        >>= (fromString' >>> lmap (ForeignError >>> NonEmptyList.singleton) >>> except)

derive instance Generic Platform _

instance showPlatform :: Show Platform where
    show = genericShow

type Platforms = { head :: Platform, tail :: Array Platform }

type PlayerProfileRow fields =
    ( platforms :: Platforms
    , platform :: Platform
    , fieldValues :: Array
        { field ::
            { ilk :: Int
            , key :: String
            , label :: String
            , icon :: String
            }
        , url :: Maybe String
        , option :: Maybe
            { key :: String
            , label :: String
            }
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    , about :: Array String
    , ambitions :: Array String
    , newOrReturning :: Boolean
    , updated :: String
    , updatedSeconds :: Number
    | fields
    )

type PlayerDetailsRow fields =
    ( age :: Maybe Int
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayOnline :: Maybe { from :: String, to :: String }
    , weekendOnline :: Maybe { from :: String, to :: String }
    | fields
    )

type PlayerContactsRow fields =
    ( discordTag :: Maybe String
    , steamId :: Maybe String
    , riotId :: Maybe String
    , battleTag :: Maybe String
    , eaId :: Maybe String
    , ubisoftUsername :: Maybe String
    , psnId :: Maybe String
    , gamerTag :: Maybe String
    , friendCode :: Maybe String
    | fields
    )

type PlayerBaseRow fields =
    ( nickname :: String
    | fields
    )

type OkContentProfiles = Record (PlayerBaseRow + PlayerContactsRow + PlayerDetailsRow + PlayerProfileRow + ())

type OkContent =
    { profiles :: Array OkContentProfiles
    , count :: Int
    }

type TestRoute = FullRoute Get TestPath TestQuery (BadRequest OkContent :! Ok OkContent)

fetchTest ::
  Async String
    (Variant
       ( badRequest :: OkContent
       , ok :: OkContent
       )
    )
fetchTest = fetch (Proxy :: _ TestRoute) { body: unit, pathParameters: testPathParameters, queryParameters: testQueryParameters } defaultOptions { origin = Just "https://www.teamtavern.net", pathPrefix = Just "/api" }

fetchTests = do
    log requestUrlPathTest
    log requestUrlQueryTest
    log requestUrlTest
    fetchTest # runAsync (either logShow logShow)
