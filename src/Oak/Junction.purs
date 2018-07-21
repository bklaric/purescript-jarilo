module Oak.Junction where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method)
import Data.List (List)
import Record.Builder (Builder, build, insert)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..), Variant, inj)
import Prim.Row (class Cons, class Lacks)
import Oak.Route (class RouteRouter, RouteErrors, RouteProxy(..), routeRouter, kind Route)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)
import URI.Path.Segment (PathSegment)

foreign import kind Junction

foreign import data NamedRoute :: Symbol -> Route -> Junction

infixr 9 type NamedRoute as :=

foreign import data Junction :: Junction -> Junction -> Junction

infixr 8 type Junction as :<|>

data JunctionProxy (junction :: Junction) = JunctionProxy

class JunctionRouter
    (junction :: Junction) (start :: # Type) (end :: # Type) (records :: # Type)
    | junction -> start end records where
    junctionRouter
        :: JunctionProxy junction
        -> Either CustomMethod Method
        -> List PathSegment
        -> QueryPairs Key Value
        -> Either
            (Builder (Record start) (Record end))
            (Variant records)

instance junctionRouterNamedRoute ::
    ( RouteRouter route fields
    , Lacks name start
    , Cons name (Variant RouteErrors) start end
    , Cons name (Record fields) inputRecords records
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

router
    :: forall junction errors results
    .  JunctionRouter junction () errors results
    => JunctionProxy junction
    -> Either CustomMethod Method
    -> List PathSegment
    -> QueryPairs Key Value
    -> Either (Record errors) (Variant results)
router junctionProxy method path query =
    junctionRouter junctionProxy method path query
    # lmap (flip build {})
