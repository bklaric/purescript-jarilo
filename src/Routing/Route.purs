module Routing.Route where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (Method)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Record.Builder (build)
import Data.StrMap (StrMap)
import Data.Variant (Variant)
import Routing.Method (class MethodRouter, MethodError, MethodProxy(MethodProxy), methodRouter, kind Method)
import Routing.Path (class PathRouter, PathError, PathProxy(PathProxy), pathRouter, kind Path)
import Routing.Query (class QueryRouter, QueryError, QueryProxy(QueryProxy), queryRouter, kind Query)
import Routing.Segment (SegmentError)

foreign import kind Route

foreign import data Route :: Method -> Path -> Query -> Route

data RouteProxy (route :: Route) = RouteProxy

type RouteErrors =
    ( methodError :: MethodError
    , segmentError :: SegmentError
    , pathError :: PathError
    , queryError :: QueryError
    )

class RouteRouter (route :: Route) (fields :: # Type) | route -> fields where
    routeRouter
        :: RouteProxy route
        -> Method
        -> List String
        -> StrMap String
        -> Either (Variant RouteErrors) (Record fields)

instance routeRouterRoute ::
    ( MethodRouter method
    , PathRouter path () midput
    , QueryRouter query midput fields
    ) =>
    RouteRouter (Route method path query) fields where
    routeRouter _ method path query =
        case methodRouter (MethodProxy :: MethodProxy method) method of
        Just methodError -> Left methodError
        Nothing -> do
            pathBuilder <- pathRouter (PathProxy :: PathProxy path) path
            queryBuilder <- queryRouter (QueryProxy :: QueryProxy query) query
            pure $ build (pathBuilder >>> queryBuilder) {}
