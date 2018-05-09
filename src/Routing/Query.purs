module Routing.Query where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Record.Builder (Builder, insert, passThrough)
import Data.StrMap (StrMap, delete, lookup)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Routing.Segment (class FromSegment, fromSegment)
import Type.Row (class RowLacks)

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
        , query :: StrMap String
        }
    | ParameterParseError
        { parameterName :: String
        , errorMessage :: String
        , actualValue :: String
        }

class QueryRouter (query :: Query) (input :: # Type) (output :: # Type)
    | query -> input output where
    queryRouter
        :: forall errors
        .  QueryProxy query
        -> StrMap String
        -> Either
            (Variant (queryError :: QueryError | errors))
            (Tuple (StrMap String) (Builder (Record input) (Record output)))

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
        key = (SProxy :: SProxy name)
        newQuery = delete (reflectSymbol key) query
        in do
        keyBuilder <-
            case lookup (reflectSymbol key) query of
            Nothing -> Right $ insert key Nothing
            Just value -> fromSegment value # bimap
                ({ parameterName: reflectSymbol key, errorMessage: _, actualValue: value }
                    >>> ParameterParseError
                    >>> inj (SProxy :: SProxy "queryError"))
                (Just >>> insert key)
        pure $ Tuple newQuery keyBuilder

instance queryRouterMandatory ::
    ( IsSymbol name
    , FromSegment result
    , RowLacks name input
    , RowCons name result input output
    ) =>
    QueryRouter (Mandatory name result) input output where
    queryRouter _ query = let
        key = (SProxy :: SProxy name)
        newQuery = delete (reflectSymbol key) query
        in do
        keyBuilder <-
            case lookup (reflectSymbol key) query of
            Nothing -> Left
                $ inj (SProxy :: SProxy "queryError")
                $ MissingParameterError { parameterName: reflectSymbol key, query: query }
            Just value -> fromSegment value # bimap
                ({ parameterName: reflectSymbol key, errorMessage: _, actualValue: value }
                    >>> ParameterParseError
                    >>> inj (SProxy :: SProxy "queryError"))
                (insert key)
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
