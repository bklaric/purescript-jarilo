module Test.Main where

import Prelude

import Data.Either (Either(..), hush)
import Data.HTTP.Method (Method(..))
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Jarilo.Junction (type (:=), type (:<|>), router)
import Jarilo.Method (Get, Post)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (type (:?), Mandatory, NoQuery, Optional)
import Jarilo.Route (FullRoute, RouteErrors)
import Type.Proxy (Proxy(..))
import URI.Extra.QueryPairs (QueryPairs(..))
import URI.Path.Segment (unsafeSegmentFromString)

type RegisterPlayer = FullRoute Post (Literal "players") NoQuery

type ViewPlayer = FullRoute Get (Literal "players" :> Capture "nickname" NonEmptyString) NoQuery

type ViewPlayers = FullRoute Get (Literal "players") (Optional "game" NonEmptyString :? Mandatory "teamId" Int)

type PlayerRoutes
    =    "registerPlayer" := RegisterPlayer
    :<|> "viewPlayer"     := ViewPlayer
    :<|> "viewPlayers"    := ViewPlayers

junction :: forall t166.
    Either
        { viewPlayers :: Variant RouteErrors
        , viewPlayer :: Variant RouteErrors
        , registerPlayer :: Variant RouteErrors
        }
        (Variant
            ( registerPlayer :: {}
            , viewPlayer :: { nickname :: NonEmptyString }
            , viewPlayers :: { teamId :: Int, game :: Maybe NonEmptyString }
            | t166
            )
        )
junction =
    router (Proxy :: _ PlayerRoutes) (Right POST) (unsafeSegmentFromString "players" : List.Nil) (QueryPairs [])

wut :: String
wut = case hush junction of
    Nothing -> "nothing"
    Just routeValues -> match
        { registerPlayer: \{} -> "register player"
        , viewPlayer: \{ nickname } -> "view player " <> show nickname
        , viewPlayers: \{ teamId, game } -> "viewPlayers " <> show teamId <> " " <> show game
        }
        routeValues

main :: Effect Unit
main = do
  log "You should add some tests."
    