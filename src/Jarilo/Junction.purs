module Jarilo.Junction where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method)
import Data.List (List)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, inj)
import Jarilo.Route (class RouteRouter, Route, RouteErrors, routeRouter)
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder, build, insert)
import Type.Proxy (Proxy(..))
import URI.Extra.QueryPairs (Key, QueryPairs, Value)
import URI.Path.Segment (PathSegment)

foreign import data Junction :: Type

foreign import data NamedRoute :: Symbol -> Route -> Junction

infixr 9 type NamedRoute as :=

foreign import data JunctionChain :: Junction -> Junction -> Junction

infixr 8 type JunctionChain as :<|>

class JunctionRouter
    (junction :: Junction) (start :: Row Type) (end :: Row Type) (records :: Row Type)
    | junction -> start end records where
    junctionRouter
        :: Proxy junction
        -> Either CustomMethod Method
        -> List PathSegment
        -> QueryPairs Key Value
        -> Either
            (Builder (Record start) (Record end))
            (Variant records)

instance
    ( RouteRouter route fields
    , Lacks name start
    , Cons name (Variant RouteErrors) start end
    , Cons name (Record fields) inputRecords records
    , IsSymbol name
    ) =>
    JunctionRouter (NamedRoute name route) start end records where
    junctionRouter _ method path query =
        case routeRouter (Proxy :: _ route) method path query of
        Left routeError -> Left $ insert (Proxy :: _ name) routeError
        Right resultRecord -> Right $ inj (Proxy :: _ name) resultRecord

instance
    ( JunctionRouter leftJunction start mid records
    , JunctionRouter rightJunction mid end records
    ) =>
    JunctionRouter (JunctionChain leftJunction rightJunction) start end records where
    junctionRouter _ method path query = let
        leftProxy = (Proxy :: _ leftJunction)
        rightProxy = (Proxy :: _ rightJunction)
        in
        case junctionRouter leftProxy method path query of
        Left leftBuilder ->
            case junctionRouter rightProxy method path query of
            Left rightBuilder -> Left $ leftBuilder >>> rightBuilder
            Right rightRecord -> Right rightRecord
        Right leftRecord -> Right leftRecord

router
    :: forall junction errors results
    .  JunctionRouter junction () errors results
    => Proxy junction
    -> Either CustomMethod Method
    -> List PathSegment
    -> QueryPairs Key Value
    -> Either (Record errors) (Variant results)
router junctionProxy method path query =
    junctionRouter junctionProxy method path query
    # lmap (flip build {})
