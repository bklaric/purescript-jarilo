module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either)
import Data.HTTP.Method (Method(..))
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.StrMap (empty)
import Data.String.NonEmpty (NonEmptyString)
import Data.Variant (Variant)
import Routing.Junction (type (:=), type (<||>), JunctionProxy(..), junctionRouter')
import Routing.Method (Get, Post)
import Routing.Path (type (:>), End)
import Routing.Query (NoQuery, Optional)
import Routing.Route (Route, RouteErrors)
import Routing.Segment (Capture, Literal)

type RegisterPlayer = Route Post (Literal "players" :> End) NoQuery

type ViewPlayer = Route Get (Literal "players" :> Capture "nickname" NonEmptyString :> End) NoQuery

type ViewPlayers = Route Get (Literal "players" :> End) (Optional "game" NonEmptyString)

type PlayerRoutes
    =    "registerPlayer" := RegisterPlayer
    <||> "viewPlayer"     := ViewPlayer
    <||> "viewPlayers"    := ViewPlayers

junction :: forall t166.
  Either
    { viewPlayers :: Variant RouteErrors
    , viewPlayer :: Variant RouteErrors
    , registerPlayer :: Variant RouteErrors
    }
    (Variant
       ( registerPlayer :: {}
       , viewPlayer :: { nickname :: NonEmptyString
                       }
       , viewPlayers :: { game :: Maybe NonEmptyString
                        }
       | t166
       )
    )
junction =
    junctionRouter' (JunctionProxy :: JunctionProxy PlayerRoutes) POST ("players" : List.Nil) empty

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."
