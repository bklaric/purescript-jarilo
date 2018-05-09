module Routing.Query where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Record.Builder (Builder, insert, passThrough)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(Tuple))
import Data.Variant (Variant, inj)
import Routing.Segment (class FromSegment, fromSegment)
import Routing.Query.QueryPairs (delete, find)
import Type.Row (class RowLacks)
import URI.Extra.QueryPairs (Key, QueryPairs, Value, keyFromString, valueToString)

foreign import kind Query

foreign import data NoQuery :: Query

foreign import data Optional :: Symbol -> Type -> Query

foreign import data Mandatory :: Symbol -> Type -> Query

foreign import data Query :: Query -> Query -> Query

infixr 9 type Query as :?

data QueryProxy (query :: Query) = QueryProxy

data QueryError
    = MissingParameterError
        { parameterName :: String
        , query :: QueryPairs Key Value
        }
    | ParameterParseError
        { parameterName :: String
        , errorMessage :: String
        , actualValue :: Value
        }

class QueryRouter (query :: Query) (input :: # Type) (output :: # Type)
    | query -> input output where
    queryRouter
        :: forall errors
        .  QueryProxy query
        -> QueryPairs Key Value
        -> Either
            (Variant (queryError :: QueryError | errors))
            (Tuple (QueryPairs Key Value) (Builder (Record input) (Record output)))

instance queryRouterNoQuery :: QueryRouter NoQuery input input where
    queryRouter _ query = pure $ Tuple query passThrough

instance queryRouterOptional ::
    ( IsSymbol name
    , FromSegment result
    , RowLacks name input
    , RowCons name (Maybe result) input output
    ) =>
    QueryRouter (Optional name result) input output where
    queryRouter _ query = let
        nameProxy = (SProxy :: SProxy name)
        newQuery = delete (keyFromString $ reflectSymbol nameProxy) query
        in do
        keyBuilder <-
            case find (keyFromString $ reflectSymbol nameProxy) query of
            Nothing -> Right $ insert nameProxy Nothing
            Just (Tuple key Nothing) -> Right $ insert nameProxy Nothing
            Just (Tuple key (Just value)) -> value # valueToString # fromSegment # bimap
                ({ parameterName: reflectSymbol nameProxy, errorMessage: _, actualValue: value }
                    >>> ParameterParseError
                    >>> inj (SProxy :: SProxy "queryError"))
                (Just >>> insert nameProxy)
        pure $ Tuple newQuery keyBuilder

instance queryRouterMandatory ::
    ( IsSymbol name
    , FromSegment result
    , RowLacks name input
    , RowCons name result input output
    ) =>
    QueryRouter (Mandatory name result) input output where
    queryRouter _ query = let
        nameProxy = (SProxy :: SProxy name)
        newQuery = delete (keyFromString $ reflectSymbol nameProxy) query
        in do
        keyBuilder <-
            case find (keyFromString $ reflectSymbol nameProxy) query of
            Nothing -> Left
                $ inj (SProxy :: SProxy "queryError")
                $ MissingParameterError { parameterName: reflectSymbol nameProxy, query: query }
            Just (Tuple key Nothing) -> Left
                $ inj (SProxy :: SProxy "queryError")
                $ MissingParameterError { parameterName: reflectSymbol nameProxy, query: query }
            Just (Tuple key (Just value)) -> value # valueToString # fromSegment # bimap
                ({ parameterName: reflectSymbol nameProxy, errorMessage: _, actualValue: value }
                    >>> ParameterParseError
                    >>> inj (SProxy :: SProxy "queryError"))
                (insert nameProxy)
        pure $ Tuple newQuery keyBuilder

instance queryRouterQuery ::
    ( QueryRouter leftQuery input midput
    , QueryRouter rightQuery midput output
    ) => QueryRouter (Query leftQuery rightQuery) input output where
    queryRouter _ query = let
        leftQueryProxy = (QueryProxy :: QueryProxy leftQuery)
        rightQueryProxy = (QueryProxy :: QueryProxy rightQuery)
        in do
        Tuple leftQuery leftBuilder <- queryRouter leftQueryProxy query
        Tuple rightQuery rightBuilder <- queryRouter rightQueryProxy leftQuery
        pure $ Tuple rightQuery $ leftBuilder >>> rightBuilder
