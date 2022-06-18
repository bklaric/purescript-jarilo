module Jarilo.Route
  ( FullRoute
  , Route
  , RouteErrors
  , class RouteRouter
  , routeRouter
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method) as HM
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Jarilo.Method (class MethodRouter, Method, MethodError, methodRouter)
import Jarilo.Path (class PathRouter, Path, PathError, pathRouter)
import Jarilo.Query (class QueryRouter, Query, QueryError, queryRouter)
import Jarilo.Segment (SegmentError)
import Record.Builder (build)
import Type.Proxy (Proxy(..))
import URI.Extra.QueryPairs (Key, QueryPairs, Value)
import URI.Path.Segment (PathSegment)

foreign import data Route :: Type

foreign import data FullRoute :: Method -> Path -> Query -> Route

type RouteErrors =
    ( methodError :: MethodError
    , segmentError :: SegmentError
    , pathError :: PathError
    , queryError :: QueryError
    )

class RouteRouter (route :: Route) (fields :: Row Type) | route -> fields where
    routeRouter
        :: Proxy route
        -> Either HM.CustomMethod HM.Method
        -> List PathSegment
        -> QueryPairs Key Value
        -> Either (Variant RouteErrors) (Record fields)

instance
    ( MethodRouter method
    , PathRouter path () midput
    , QueryRouter query midput fields
    ) =>
    RouteRouter (FullRoute method path query) fields where
    routeRouter _ method path query = let
        methodProxy = (Proxy :: _ method)
        pathProxy = (Proxy :: _ path)
        queryProxy = (Proxy :: _ query)
        in
        case methodRouter methodProxy method of
        Just methodError -> Left methodError
        Nothing -> do
            pathBuilder <- pathRouter pathProxy path
            Tuple _ queryBuilder <- queryRouter queryProxy query
            pure $ build (pathBuilder >>> queryBuilder) {}
