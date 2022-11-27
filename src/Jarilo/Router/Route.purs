module Jarilo.Router.Route where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.HTTP.Method (CustomMethod, Method) as HM
import Data.List (List)
import Data.Variant (Variant, inj)
import Jarilo.Route (FullRoute, Route)
import Jarilo.Router.Method (class MethodRouter, MethodError, methodRouter)
import Jarilo.Router.Path (class PathRouter, PathError, pathRouter')
import Jarilo.Router.Query (class QueryRouter, QueryError, queryRouter')
import Record.Builder (build)
import Type.Proxy (Proxy(..))
import URI.Extra.QueryPairs (Key, QueryPairs, Value)
import URI.Path.Segment (PathSegment)

type RouteError = Variant
    ( methodError :: MethodError
    , pathError :: PathError
    , queryError :: QueryError
    )

type RouteResult pathParameters queryParameters body =
    { path :: Record pathParameters
    , query :: Record queryParameters
    , body :: body
    }

class RouteRouter (route :: Route) pathParameters queryParameters body | route -> pathParameters queryParameters body where
    routeRouter
        :: Proxy route
        -> Either HM.CustomMethod HM.Method
        -> List PathSegment
        -> QueryPairs Key Value
        -> String
        -> Either RouteError (RouteResult pathParameters queryParameters body)

instance
    ( MethodRouter method body
    , PathRouter path () pathParameters
    , QueryRouter query () queryParameters
    ) =>
    RouteRouter (FullRoute method path query responses) pathParameters queryParameters body where
    routeRouter _ method path query body' = let
        methodProxy = (Proxy :: _ method)
        pathProxy = (Proxy :: _ path)
        queryProxy = (Proxy :: _ query)
        in do
        body <- methodRouter methodProxy method body' # lmap (inj (Proxy :: _ "methodError"))
        pathBuilder <- pathRouter' pathProxy path # lmap (inj (Proxy :: _ "pathError"))
        queryBuilder <- queryRouter' queryProxy query # lmap (inj (Proxy :: _ "queryError"))
        pure {
            path: build pathBuilder {},
            query: build queryBuilder {},
            body
        }
