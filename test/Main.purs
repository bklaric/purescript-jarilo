module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either, hush)
import Data.HTTP.Method (Method(..))
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.Variant (Variant, match)
import Routing.Junction (type (:=), type (:<|>), JunctionProxy(..), junctionRouter')
import Routing.Method (Get, Post)
import Routing.Path (type (:>), End)
import Routing.Query (type (:?), Mandatory, NoQuery, Optional)
import Routing.Route (Route, RouteErrors)
import Routing.Segment (Capture, Literal)
import URI.Extra.QueryPairs (QueryPairs(..))
import URI.Path.Segment (unsafeSegmentFromString)

type RegisterPlayer = Route Post (Literal "players" :> End) NoQuery

type ViewPlayer = Route Get (Literal "players" :> Capture "nickname" NonEmptyString :> End) NoQuery

type ViewPlayers = Route Get (Literal "players" :> End) (Optional "game" NonEmptyString :? Mandatory "teamId" Int)

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
    junctionRouter' (JunctionProxy :: JunctionProxy PlayerRoutes) POST (unsafeSegmentFromString "players" : List.Nil) (QueryPairs [])

wut :: String
wut = case hush junction of
    Nothing -> "nothing"
    Just routeValues -> match
        { registerPlayer: \{} -> "register player"
        , viewPlayer: \{ nickname } -> "view player " <> show nickname
        , viewPlayers: \{ teamId, game } -> "viewPlayers " <> show teamId <> " " <> show game
        }
        routeValues

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."
