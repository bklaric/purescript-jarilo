module Routing.Route where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (Method)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Record.Builder (Builder)
import Data.StrMap (StrMap)
import Data.Variant (Variant)
import Routing.Method (class MethodRouter, MethodError, MethodProxy(MethodProxy), methodRouter, kind Method)
import Routing.Path (class PathRouter, PathError, PathProxy(..), pathRouter, kind Path)
import Routing.Query (class QueryRouter, QueryError, QueryProxy(..), queryRouter, kind Query)
import Routing.Segment (SegmentError)

foreign import kind Route

foreign import data Route :: Method -> Path -> Query -> Route

data RouteProxy (route :: Route) = RouteProxy

type RouteErrors errors =
    ( methodError :: MethodError
    , segmentError :: SegmentError
    , pathError :: PathError
    , queryError :: QueryError
    | errors )

type RoutingResult errors input output =
    Either (Variant errors) (Builder (Record input) (Record output))

class RouteRouter
    (route :: Route) (input :: # Type) (output :: # Type)
    | route -> input output where
    routeRouter
        :: forall errors
        .  RouteProxy route
        -> Method
        -> List String
        -> StrMap String
        -> RoutingResult (RouteErrors errors) input output

instance routeRouterRoute ::
    ( MethodRouter method
    , PathRouter path input midput
    , QueryRouter query midput output
    ) =>
    RouteRouter (Route method path query) input output where
    routeRouter _ method path query =
        case methodRouter (MethodProxy :: MethodProxy method) method of
        Just error -> Left error
        Nothing -> do
            pathBuilder :: Builder (Record input) (Record midput) <-
                pathRouter (PathProxy :: PathProxy path) path
            queryBuilder :: Builder (Record midput) (Record output) <-
                queryRouter (QueryProxy :: QueryProxy query) query
            pure $ pathBuilder >>> queryBuilder
