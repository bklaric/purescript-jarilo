module Routing.Route where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Record as Record
import Data.Record.Builder (Builder, build, insert, merge)
import Data.StrMap (StrMap, empty)
import Data.String.NonEmpty (NonEmptyString)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Data.Variant (Variant, inj)
import Routing.Method (class MethodRouter, Get, MethodError, MethodProxy(MethodProxy), Post, methodRouter, kind Method)
import Routing.Path (class PathRouter, type (:>), End, PathError, PathProxy(..), pathRouter, kind Path)
import Routing.Query (class QueryRouter, Nil, QueryError, QueryProxy(..), queryRouter, kind Query)
import Routing.Segment (Capture, Literal, SegmentError)
import Type.Row (class RowLacks)

foreign import kind Route

foreign import data Route :: Symbol -> Method -> Path -> Query -> Route

foreign import data Junction :: Route -> Route -> Route

infixr 9 type Junction as <||>

data RouteProxy (route :: Route) = RouteProxy

type RouteErrors errors =
    ( methodError :: MethodError
    , segmentError :: SegmentError
    , pathError :: PathError
    , queryError :: QueryError
    | errors )

class RouteRouter (route :: Route) (input :: # Type) (start :: # Type) (end :: # Type) (records :: # Type)
    | route -> input records where
    routeRouter
        :: forall errors
        .  RouteProxy route
        -> Record input
        -> Method
        -> List String
        -> StrMap String
        -> Either
            (Builder (Record start) (Record end)) --(Variant (RouteErrors errors))
            (Variant records)

instance routeRouterRoute ::
    ( MethodRouter method
    , PathRouter path input midput
    , QueryRouter query midput fields
    , RowCons name (Record fields) otherRecords records
    , IsSymbol name
    , RowLacks name start
    ,    Prim.RowCons name
                 (Variant
                    ( methodError :: MethodError
                    , segmentError :: SegmentError
                    , pathError :: PathError
                    , queryError :: QueryError)
                 )
                 start
                 end

    ) =>
    RouteRouter (Route name method path query) input start end records where
    routeRouter _ input method path query =
        case methodRouter (MethodProxy :: MethodProxy method) method of
        Just error -> Left $ insert (SProxy :: SProxy name) error
        Nothing -> do
            pathBuilder <- pathRouter (PathProxy :: PathProxy path) path
                # lmap (insert (SProxy :: SProxy name))
            queryBuilder <- queryRouter (QueryProxy :: QueryProxy query) query
                # lmap (insert (SProxy :: SProxy name))
            pure $ inj (SProxy :: SProxy name) $ build (pathBuilder >>> queryBuilder) input

instance routeRouterJunction ::
    ( RouteRouter leftRoute input start end1 records
    , RouteRouter rightRoute input end1 end records
    ) =>
    RouteRouter (Junction leftRoute rightRoute) input start end records where
    routeRouter _ input method path query =
        case routeRouter (RouteProxy :: RouteProxy leftRoute) input method path query of
        Left (leftError :: Builder (Record start) (Record end1)) -> case routeRouter (RouteProxy :: RouteProxy rightRoute) input method path query of
            Left (rightError ) -> Left (leftError >>> rightError)
            Right rightRecord -> Right rightRecord
        Right leftRecord -> Right leftRecord

type RegisterPlayer = Route "registerPlayer" Post (Literal "players" :> End) Nil

type ViewPlayer = Route "viewPlayer" Get (Literal "players" :> Capture "nickname" NonEmptyString :> End) Nil

type PlayerRoutes = RegisterPlayer <||> ViewPlayer

routing :: forall t127.
  Either
    { viewPlayer :: Variant
                      ( methodError :: MethodError
                      , segmentError :: SegmentError
                      , pathError :: PathError
                      , queryError :: QueryError
                      )
    }
    (Variant
       ( viewPlayer :: { nickname :: NonEmptyString
                       , lolwut :: String
                       }
       | t127
       )
    )
routing =
    routeRouter
        (RouteProxy :: RouteProxy ViewPlayer)
        { lolwut: "aoeu" }
        POST
        ("players" : List.Nil)
        empty
    # lmap (flip build {})

junction :: forall t216.
  Either
    { viewPlayer :: Variant
                      ( methodError :: MethodError
                      , segmentError :: SegmentError
                      , pathError :: PathError
                      , queryError :: QueryError
                      )
    , registerPlayer :: Variant
                          ( methodError :: MethodError
                          , segmentError :: SegmentError
                          , pathError :: PathError
                          , queryError :: QueryError
                          )
    }
    (Variant
       ( registerPlayer :: { lolwut :: String
                           }
       , viewPlayer :: { nickname :: NonEmptyString
                       , lolwut :: String
                       }
       | t216
       )
    )
junction =
    routeRouter
        (RouteProxy :: RouteProxy PlayerRoutes)
        { lolwut: "aoeu" }
        POST
        ("players" : List.Nil)
        empty
    # lmap (flip build {})
