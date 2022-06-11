module Jarilo.Route where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method) as HM
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Jarilo.Method (class MethodRouter, MethodError, methodRouter, Method)
import Jarilo.Path (class PathRouter, PathError, pathRouter, Path)
import Jarilo.Query (class QueryRouter, QueryError, queryRouter, Query)
import Jarilo.Segment (SegmentError)
import Record.Builder (build)
import Type.Proxy (Proxy(..))
import URI.Extra.QueryPairs (Key, QueryPairs, Value)
import URI.Path.Segment (PathSegment)

foreign import data Route :: Type

foreign import data Route' :: Method -> Path -> Query -> Route

type RouteErrors =
    ( methodError :: MethodError
    , segmentError :: SegmentError
    , pathError :: PathError
    , queryError :: QueryError
    )

-- routeRouter
--     :: forall route fields method path query midput
--     .  MethodRouter method
--     => PathRouter path () midput
--     => QueryRouter query midput fields
--     => Proxy route
--     -> Either HM.CustomMethod HM.Method
--     -> List PathSegment
--     -> QueryPairs Key Value
--     -> Either (Variant RouteErrors) (Record fields)
-- routeRouter _ method path query = let
--     methodProxy = (Proxy :: _ method)
--     pathProxy = (Proxy :: _ path)
--     queryProxy = (Proxy :: _ query)
--     in
--     case methodRouter methodProxy method of
--     Just methodError -> Left methodError
--     Nothing -> do
--         pathBuilder <- pathRouter pathProxy path
--         Tuple _ queryBuilder <- queryRouter queryProxy query
--         pure $ build (pathBuilder >>> queryBuilder) {}

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
    RouteRouter (Route' method path query) fields where
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
