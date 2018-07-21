module Jarilo.Route where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Record.Builder (build)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Jarilo.Method (class MethodRouter, MethodError, MethodProxy(..), methodRouter, kind Method)
import Jarilo.Path (class PathRouter, PathError, PathProxy(..), pathRouter, kind Path)
import Jarilo.Query (class QueryRouter, QueryError, QueryProxy(..), queryRouter, kind Query)
import Jarilo.Segment (SegmentError)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)
import URI.Path.Segment (PathSegment)

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
        -> Either CustomMethod Method
        -> List PathSegment
        -> QueryPairs Key Value
        -> Either (Variant RouteErrors) (Record fields)

instance routeRouterRoute ::
    ( MethodRouter method
    , PathRouter path () midput
    , QueryRouter query midput fields
    ) =>
    RouteRouter (Route method path query) fields where
    routeRouter _ method path query = let
        methodProxy = (MethodProxy :: MethodProxy method)
        pathProxy = (PathProxy :: PathProxy path)
        queryProxy = (QueryProxy :: QueryProxy query)
        in
        case methodRouter methodProxy method of
        Just methodError -> Left methodError
        Nothing -> do
            pathBuilder <- pathRouter pathProxy path
            Tuple _ queryBuilder <- queryRouter queryProxy query
            pure $ build (pathBuilder >>> queryBuilder) {}
