module Jarilo.Route where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method) as HM
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Jarilo.Method (class MethodRouter, Method, MethodError, methodRouter)
import Jarilo.Path (class PathRouter, Path, PathError(..), pathRouter)
import Jarilo.Query (class QueryRouter, Query, QueryError, queryRouter)
import Jarilo.Response (Response)
import Record.Builder (build)
import Type.Proxy (Proxy(..))
import URI.Extra.QueryPairs (Key, QueryPairs, Value)
import URI.Path.Segment (PathSegment)

foreign import data Route :: Type

foreign import data FullRoute :: Method -> Path -> Query -> Response -> Route

type RouteErrors =
    ( methodError :: MethodError
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
    RouteRouter (FullRoute method path query responses) fields where
    routeRouter _ method path query = let
        methodProxy = (Proxy :: _ method)
        pathProxy = (Proxy :: _ path)
        queryProxy = (Proxy :: _ query)
        in
        case methodRouter methodProxy method of
        Just methodError -> Left methodError
        Nothing -> do
            Tuple restOfPath pathBuilder <- pathRouter pathProxy path
            case restOfPath of
                Nil -> do
                    Tuple _ queryBuilder <- queryRouter queryProxy query
                    pure $ build (pathBuilder >>> queryBuilder) {}
                _ -> Left $ inj (Proxy :: _ "pathError") $ NotEndError { restOfPath }
