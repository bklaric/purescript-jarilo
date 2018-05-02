module Routing.Junction where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method)
import Data.List (List)
import Data.Record.Builder (Builder, build, insert)
import Data.StrMap (StrMap)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..), Variant, inj)
import Routing.Route (class RouteRouter, RouteErrors, RouteProxy(RouteProxy), routeRouter, kind Route)
import Type.Row (class RowLacks)

foreign import kind Junction

foreign import data NamedRoute :: Symbol -> Route -> Junction

infixr 9 type NamedRoute as :=

foreign import data Junction :: Junction -> Junction -> Junction

infixr 8 type Junction as <||>

data JunctionProxy (junction :: Junction) = JunctionProxy

class JunctionRouter
    (junction :: Junction) (start :: # Type) (end :: # Type) (records :: # Type)
    | junction -> start end records where
    junctionRouter
        :: JunctionProxy junction
        -> Method
        -> List String
        -> StrMap String
        -> Either
            (Builder (Record start) (Record end))
            (Variant records)

instance junctionRouterNamedRoute ::
    ( RouteRouter route fields
    , RowLacks name start
    , RowCons name (Variant RouteErrors) start end
    , RowCons name (Record fields) inputRecords records
    , IsSymbol name
    ) =>
    JunctionRouter (NamedRoute name route) start end records where
    junctionRouter _ method path query =
        case routeRouter (RouteProxy :: RouteProxy route) method path query of
        Left routeError -> Left $ insert (SProxy :: SProxy name) routeError
        Right resultRecord -> Right $ inj (SProxy :: SProxy name) resultRecord

instance junctionRouterJunction ::
    ( JunctionRouter leftJunction start mid records
    , JunctionRouter rightJunction mid end records
    ) =>
    JunctionRouter (Junction leftJunction rightJunction) start end records where
    junctionRouter _ method path query = let
        leftProxy = (JunctionProxy :: JunctionProxy leftJunction)
        rightProxy = (JunctionProxy :: JunctionProxy rightJunction)
        in
        case junctionRouter leftProxy method path query of
        Left leftBuilder ->
            case junctionRouter rightProxy method path query of
            Left rightBuilder -> Left $ leftBuilder >>> rightBuilder
            Right rightRecord -> Right rightRecord
        Right leftRecord -> Right leftRecord

junctionRouter'
    :: forall junction errors results
    .  JunctionRouter junction () errors results
    => JunctionProxy junction
    -> Method
    -> List String
    -> StrMap String
    -> Either (Record errors) (Variant results)
junctionRouter' junctionProxy method path query =
    junctionRouter junctionProxy method path query
    # lmap (flip build {})
